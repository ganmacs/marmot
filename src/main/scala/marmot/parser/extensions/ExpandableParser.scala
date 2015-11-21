package marmot.parser.extensions

import marmot._

// TODO change expandable trait
class ExpandableParser extends BasicParser {
  var xParsers: PMap = new PMap
  var _namespace: Option[String] = None
  private def p: ExpandableParser = _namespace match {
    case None => this
    case Some(v) => xParsers.getOrCreateBy(v)
  }

  private def convertToParsers(exprs: List[Expr], env: Env[Expr]): List[Parser[Expr]] =
    exprs.map {
      case VarLit(x) => x ^^ { case e => Empty() }
      case IntLit(_) => p.int.asInstanceOf[Parser[Expr]]
      case DoubleLit(_) => p.double.asInstanceOf[Parser[Expr]]
      case BoolLit(_) => p.bool.asInstanceOf[Parser[Expr]]
      case Prim(x, e1, e2) => p.bool.asInstanceOf[Parser[Expr]]
      case OperatorVar(VarLit(x))=> p.expr.asInstanceOf[Parser[Expr]] ^^ { case e => env.put(x, e); Empty() }
      case _ => "" ^^^ Empty()
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

  def doInNS(ns: String, f: => Unit) = {
    _namespace = Some(ns); f; _namespace = None
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

case class PMap (var m: Map[String, ExpandableParser] = Map.empty[String, ExpandableParser]) {
  def apply(key: String) = m.apply(key)
  def put(k: String, v: ExpandableParser) = { m = Map(k -> v) ++ m; this }
  def get(k: String): Option[ExpandableParser] = m.get(k)
  def getOrCreateBy(k: String): ExpandableParser = get(k) match {
    case None => {
      val p = new ExpandableParser
      put(k, p)
      p
    }
    case Some(m) => m
  }

  def getOrElseIn(k: String, t: ExpandableParser): ExpandableParser = get(k) match {
    case None => t
    case Some(m) => m
  }

  override def toString = {
    var s = "( "
    for ((k,v) <- m) {
      s += s"$k => $v, "
    }
    s+")"
  }
}
