package marmot.parser.extensions

import marmot._

// TODO change expandable trait
class ExpandableParser extends BasicParser {
  var xParser: Option[ExpandableParser] = None
  private lazy val p: ExpandableParser = xParser.getOrElse(this)

  private def convertToParsers(exprs: List[Expr], env: Env[Expr]): List[Parser[Expr]] =
    exprs.map {
      case VarLit(x) => x ^^ { case e => Empty() }
      case IntLit(_) => p.int.asInstanceOf[Parser[Expr]]
      case DoubleLit(_) => p.double.asInstanceOf[Parser[Expr]]
      case BoolLit(_) => p.bool.asInstanceOf[Parser[Expr]]
      case Prim(x, e1, e2) => p.bool.asInstanceOf[Parser[Expr]]
      case OperatorVar(VarLit(x))=> p.expr.asInstanceOf[Parser[Expr]] ^^ { case e => env.put(x, e); Empty() }
      case _ => "" ^^ { case _ => Empty() }
    }

  private def expandMacro(expr: Expr, env: Env[Expr]): Expr = expr match {
    case v@VarLit(x) => env.get(x) match {
      case Some(ex) => ex
      case None => v // not macro, so return default value
    }
    case Prim(x, e1, e2) => {
      val ne1 = expandMacro(e1, env)
      val ne2 = expandMacro(e2, env)
      Prim(x, ne1, ne2)
    }
    case IfExp(cond, e1, e2) => {
      val nc = expandMacro(cond, env)
      val ne1 = expandMacro(e1, env)
      val ne2 = expandMacro(e2, env)
      IfExp(nc, ne1, ne2)
    }
    case Fun(args, body) => {
      val nargs = args.map { e => expandMacro(e, env).asInstanceOf[VarLit] }
      val nbody = expandMacro(body, env)
      Fun(nargs, nbody)
    }
    case e => e
  }

  private def buildParser(exprs: List[Expr], semantics: Expr): (PackratParser[Expr], Env[Expr]) = {
    val operatorEnv = Env.empty[Expr]
    val parsers = convertToParsers(exprs, operatorEnv)
    val _parser = parsers.tail.foldLeft(parsers.head) {
      (a, b) => a ~ b ^^^ Empty()
    }
    (_parser, operatorEnv)
  }

  def expandWith(exprs: List[Expr], semantics: Expr): Unit = {
    buildParser(exprs, semantics) match {
      case (_parser, _env) => {
        val _tmp = expr
        expr = _parser ^^ { case _ => expandMacro(semantics, _env) } | _tmp
      }
    }
  }
}
