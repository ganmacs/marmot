package marmot.parser.extensions

import marmot._

trait Expandable extends BaseParser {
  val int: PackratParser[Expr]
  val double: PackratParser[Expr]
  val id: PackratParser[VarLit]
  val bool: PackratParser[Expr]
  var expr: PackratParser[Expr]
  val fact: PackratParser[Expr]
  val term: PackratParser[Expr]

  private def convertToParsers(exprs: List[Expr], env: Env[Expr]): List[Parser[Expr]] = exprs.map {
    case VarLit(x) => x ^^ { case e => Empty() }
    case IntLit(_) => int
    case DoubleLit(_) => double
    case BoolLit(_) => bool
    case Prim(x, e1, e2) => bool
    case NoTermToken("$EXPR") => expr
    case NoTermToken("$TERM") => term
    case NoTermToken("$FACT")=> fact
    case OperatorVar(VarLit(x))=> expr ^^ { case e => env.put(x, e); Empty() }
    case MacroVar(VarLit(x), NoTermToken("$EXPR")) => expr ^^ { case e => env.put(x, e); Empty() }
    case MacroVar(VarLit(x), NoTermToken("$TERM")) => term ^^ { case e => env.put(x, e); Empty() }
    case MacroVar(VarLit(x), NoTermToken("$FACT")) => fact ^^ { case e => env.put(x, e); Empty() }
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

  def buildParser(exprs: List[Expr], semantics: Expr): (PackratParser[Expr], Env[Expr]) = {
    val operatorEnv = Env.empty[Expr]
    val parsers = convertToParsers(exprs, operatorEnv)
    val _parser = parsers.tail.foldLeft(parsers.head) {
      (a, b) => a ~ b ^^^ Empty()
    }
    return (_parser, operatorEnv)
  }

  def expand(exprs: List[Expr], semantics: Expr): Unit = {
    buildParser(exprs, semantics) match {
      case (_parser, env) => {
        val tmp = expr
        expr = _parser ^^ { case e => expandMacro(semantics, env) } | tmp
      }
    }
  }
}
