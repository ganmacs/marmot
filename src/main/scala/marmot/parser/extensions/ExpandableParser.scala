package marmot.parser.extensions

import marmot._
import scala.collection.mutable.{Map => MMap}

class ExpandableParser extends BasicParser {
  protected var parserMap: MMap[String, ExpandableParser] = MMap.empty[String, ExpandableParser]

  // Assosiate builed parser with semantics
  def expandSyntax(exprs: List[Expr], semantics: Expr): Unit = {
    buildParser(exprs) match {
      case (parser, map) => {
        val tmp = expr
        expr = parser ^^ { case _ => expandMacro(semantics, map) } | tmp
      }
    }
  }

  private def expandMacro(expr: Expr, m: MMap[String, List[Expr]]): Expr = expr match {
    case v@VarLit(x) => m.get(x) match {
      case Some(ex) => {
        if (ex.size >= 1) {
          m.put(x, ex.drop(1))
        } else {
          m -= x
        }
        ex(0)
      }
      case None => v // Not macro. Return default value.
    }
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
        // TODO may be ,passing just parent map is better
        p.parserMap.put("*", parserMap("*"))
        parserMap.put(v, p)
        p
      }
    }
  }

  // Convert Exprs to micro parsers
  private def convertExprsToParsers(exprs: List[Expr]): (List[Parser[Expr]], MMap[String, List[Expr]]) = {
    val m = MMap.empty[String, List[Expr]]
    val r = exprs.map {
      case VarLit(x) => x ^^ { case e => Empty() }
      case VarWithContext(VarLit(x), c) => p(c).expr.asInstanceOf[Parser[Expr]] ^^ {
        case v => m.get(x) match {
          case Some(xx) => m.put(x, v :: xx)
          case None => m.put(x, List(v))
        }; Empty()
      }
      case x => throw new Exception("Unknow Token: " + x)
    }
    (r, m)
  }

  // Build micro parsers and then fold them to big parser.
  private def buildParser(exprs: List[Expr]): (PackratParser[Expr], MMap[String, List[Expr]]) = {
    convertExprsToParsers(exprs) match {
      case (parsers, m) => {
        val parser = parsers.tail.foldLeft(parsers.head) { (a, b) => a ~ b ^^^ Empty() }
        (parser, m)
      }
    }
  }
}
