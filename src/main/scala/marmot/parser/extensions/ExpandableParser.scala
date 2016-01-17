package marmot.parser.extensions

import marmot._
import scala.collection.mutable.{Map => MMap}

class ExpandableParser extends BasicParser {
  protected var parserMap: MMap[String, ExpandableParser] = MMap.empty[String, ExpandableParser]

  // Assosiate builed parser with semantics
  def expandSyntax(exprs: List[Expr], semantics: Expr): Unit = {
    val (parser, map) = buildParser(exprs)
    val tmp = expr
    expr = parser ^^ { case _ => expandMacro(semantics, map) } | tmp
  }

  private def expandMacro(expr: Expr, m: MacroMap): Expr = expr match {
    case v@VarLit(x) => m.getOrElse(x, v)
    case Prim(x, e1, e2) => {
      val ne1 = expandMacro(e1, m)
      val ne2 = expandMacro(e2, m)
      Prim(x, ne1, ne2)
    }
    case IfExp(cond, e1, e2) => {
      val nc = expandMacro(cond, m)
      val ne1 = expandMacro(e1, m)
      val ne2 = expandMacro(e2, m)
      IfExp(nc, ne1, ne2)
    }
    case Fun(args, body) => {
      val nargs = args.map { e => expandMacro(e, m).asInstanceOf[VarLit] }
      val nbody = expandMacro(body, m)
      Fun(nargs, nbody)
    }
    case e => e
  }

  private def p(c :Context): ExpandableParser = {
    val Context(v) = c
    parserMap.get(v) match {
      case Some(m) => m
      case None => {
        val p = new ExpandableParser
        // initialize every parser has ("*" => OperatorParesr) in paserMap
        p.parserMap.put("*", parserMap("*"))
        p.parserMap.put(v, p)
        parserMap.put(v, p)
        p
      }
    }
  }

  // Convert Exprs to micro parsers
  private def convertExprsToParsers(exprs: List[Expr]): (List[Parser[Expr]], MacroMap) = {
    val m = new MacroMap()
    val r = exprs.map {
      case VarLit(x) => x ^^ { case e => Empty() }
      case VarWithContext(VarLit(x), c) => p(c).expr.asInstanceOf[Parser[Expr]] ^^ {
        case v => m.put(x, v); Empty()
      }
      case x => throw new Exception("Unknow Token: " + x)
    }
    (r, m)
  }

  // Build micro parsers and then fold them to big parser.
  private def buildParser(exprs: List[Expr]): (PackratParser[Expr], MacroMap) = {
    val (parsers, m) = convertExprsToParsers(exprs)
    val parser = parsers.tail.foldLeft(parsers.head) { (a, b) => a ~ b ^^^ Empty() }
    (parser, m)
  }

  // Macro map is a Map. Data structure its value is a stack.
  private class MacroMap {
    val m = MMap.empty[String, List[Expr]]

    def put(k: String, v: Expr) = m.get(k) match {
      case Some(xx) => m.put(k, v :: xx)
      case None => m.put(k, List(v))
    }

    def getOrElse(k: String, a: Expr): Expr = get(k).getOrElse(a)

    def get(k: String): Option[Expr] = m.get(k) match {
      case Some(xx) => {
        if (xx.size >= 1) {
          m.put(k, xx.drop(1))
        } else {
          m -= k
        }
        Some(xx(0))
      }
      case None => None
    }
  }
}
