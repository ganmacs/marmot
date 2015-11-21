package marmot.parser.extensions

import marmot._
import scala.collection.mutable.{Map => MMap}

// TODO change expandable trait
class ExpandableParser extends BasicParser {
  var xParsers: MMap[String, ExpandableParser] = MMap.empty[String, ExpandableParser]
  var _namespace: Option[String] = None

  private def p: ExpandableParser = _namespace match {
    case None => this
    case Some(v) => {
      xParsers.get(v) match {
        case Some(m) => m
        case None => {
          val p = new ExpandableParser
          xParsers.put(v, p)
          p
        }
      }
    }
  }

  private def convertToParsers(exprs: List[Expr], m: MMap[String, Expr]): List[Parser[Expr]] =
    exprs.map {
      case VarLit(x) => x ^^ { case e => Empty() }
      case IntLit(_) => p.int.asInstanceOf[Parser[Expr]]
      case DoubleLit(_) => p.double.asInstanceOf[Parser[Expr]]
      case BoolLit(_) => p.bool.asInstanceOf[Parser[Expr]]
      case Prim(x, e1, e2) => p.bool.asInstanceOf[Parser[Expr]]
      case OperatorVar(VarLit(x))=> p.expr.asInstanceOf[Parser[Expr]] ^^ { case e => m.put(x, e); Empty() }
      case _ => "" ^^^ Empty()
    }

  private def expandMacro(expr: Expr, m: MMap[String, Expr]): Expr = expr match {
    case v@VarLit(x) => m.get(x) match {
      case Some(ex) => ex
      case None => v // not macro, so return default value
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

  def doInNS(ns: String, f: => Unit) = {
    _namespace = Some(ns); f; _namespace = None
  }

  private def buildParser(exprs: List[Expr], semantics: Expr): (PackratParser[Expr], MMap[String, Expr]) = {
    val operatorMap = MMap.empty[String, Expr]
    val parsers = convertToParsers(exprs, operatorMap)
    val _parser = parsers.tail.foldLeft(parsers.head) {
      (a, b) => a ~ b ^^^ Empty()
    }
    (_parser, operatorMap)
  }

  def expandWith(exprs: List[Expr], semantics: Expr): Unit = {
    buildParser(exprs, semantics) match {
      case (_parser, _map) => {
        val _tmp = expr
        expr = _parser ^^ { case _ => expandMacro(semantics, _map) } | _tmp
      }
    }
  }
}
