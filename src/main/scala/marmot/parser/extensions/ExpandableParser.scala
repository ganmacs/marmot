package marmot.parser.extensions

import marmot._
import scala.collection.mutable.{Map => MMap}

class ExpandableParser extends BasicParser {
  protected var parserMap: MMap[String, ExpandableParser] = MMap.empty[String, ExpandableParser]
  private var context: Option[String] = None

  // Assosiate builed parser with semantics
  def expandSyntax(exprs: List[Expr], semantics: Expr): Unit = {
    buildParser(exprs) match {
      case (parser, map) => {
        val tmp = expr
        expr = parser ^^ { case _ => expandMacro(semantics, map) } | tmp
      }
    }
  }

  def doInNS(ns: String, f: => Unit) = {
    context = Some(ns); f; context = None
  }

  private def p: ExpandableParser = context match {
    case None => this
    case Some(v) => {
      parserMap.get(v) match {
        case Some(m) => m
        case None => {
          val p = new ExpandableParser
          parserMap.put(v, p)
          p
        }
      }
    }
  }

  private def expandMacro(expr: Expr, m: MMap[String, Expr]): Expr = expr match {
    case v@VarLit(x) => m.get(x) match {
      case Some(ex) => ex
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

  // Convert Exprs to micro parsers
  private def convertExprsToParsers(exprs: List[Expr]): (List[Parser[Expr]], MMap[String, Expr]) = {
    val m = MMap.empty[String, Expr]
    val r = exprs.map {
      case VarLit(x) => x ^^ { case e => Empty() }
      case IntLit(_) => p.int.asInstanceOf[Parser[Expr]]
      case DoubleLit(_) => p.double.asInstanceOf[Parser[Expr]]
      case BoolLit(_) => p.bool.asInstanceOf[Parser[Expr]]
      case Prim(x, e1, e2) => p.bool.asInstanceOf[Parser[Expr]]
      case OperatorVar(VarLit(x)) => p.expr.asInstanceOf[Parser[Expr]] ^^ { case v => m.put(x, v); Empty() }
      case _ => "" ^^^ Empty()
    }
    (r, m)
  }

  // Build micro parsers and then fold them to big parser.
  private def buildParser(exprs: List[Expr]): (PackratParser[Expr], MMap[String, Expr]) = {
    convertExprsToParsers(exprs) match {
      case (parsers, m) => {
        val parser = parsers.tail.foldLeft(parsers.head) { (a, b) => a ~ b ^^^ Empty() }
        (parser, m)
      }
    }
  }
}
