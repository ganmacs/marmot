package marmot.parser.extensions

import marmot._

class BasicParser extends BaseParser {
  def parse(in: String): Either[String, Prog] = parseAll(stmnt, in) match {
    case Success(d, next) => Right(d)
    case NoSuccess(errorMsg, next) =>
      Left(s"$errorMsg : in ${next.pos.line} at column ${next.pos.column}")
  }

  private def convertExprToParser(e: List[Expr], env: Env[Expr]): List[Parser[Expr]] = e.map {
    case VarLit(x) => x ^^ { case e => Empty() }
    case IntLit(_) => int
    case DoubleLit(_) => double
    case BoolLit(_) => bool
    case Prim(x, e1, e2) => bool
    case NoTermToken("$EXPR") => expr
    case NoTermToken("$TERM") => term
    case NoTermToken("$FACT")=> fact
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

  def registerRule(t: NoTermToken, exprs: List[Expr], semntics: Expr) = t match {
    case NoTermToken("$EXPR") => {
      val macroEnv = Env.empty[Expr]

      val parsers = convertExprToParser(exprs, macroEnv)

      var b = parsers.tail.foldLeft(parsers.head) {
        (a, b) => a ~ b ^^ { case a ~ b => Empty() }
      }

      val tmp = expr
      expr = b ^^ { case e => expandMacro(semntics, macroEnv) } | tmp
    }
    case NoTermToken("$TERM") =>
    case NoTermToken("$FACT") =>
    case _ => Nil
  }

  private lazy val stmnt: PackratParser[Prog] = (expr).* ^^ { case e => Prog(e) }

  private var expr: PackratParser[Expr] =
    fun | ifexp | let | app | term ~ exprR.* ^^ { case l ~ r => makeBinExpr(l, r) }

  private lazy val let: PackratParser[Let] = (LET ~> id) ~ (EQ ~> expr) ~ (IN ~> expr) ^^ {
    case id ~ value ~ body => Let(id, value, body)
  }

  private lazy val ifexp: PackratParser[IfExp] = (IF ~> expr) ~ (THEN ~> expr) ~ (ELSE ~> expr) ^^ {
    case c ~ e1 ~ e2 => IfExp(c, e1, e2)
  }

  val term: PackratParser[Expr] =
    fact ~ termR.* ^^ { case l ~ r => makeBinExpr(l, r) }

  private lazy val app: PackratParser[Expr] = id ~ (LPAREN ~> expr.* <~ RPAREN) ^^ {
    case n ~ exprs => App(n, exprs)
  }

  private lazy val fun: PackratParser[Expr] = (FUN ~> args) ~ (RARROW ~> expr) ^^ {
    case s ~ e => Fun(s, e)
  }

  private lazy val args = id.* // FIX?

  val fact: PackratParser[Expr] = bool | double | int | id | LPAREN ~> expr <~ RPAREN

  private lazy val int: PackratParser[Expr] = INT ^^ { case e => IntLit(e.toInt) }
  private lazy val double: PackratParser[Expr] = DOUBLE ^^ { case e => DoubleLit(e.toDouble) }
  private lazy val id: PackratParser[VarLit] = ID ^^ { case e => VarLit(e) }
  private lazy val bool: PackratParser[Expr] = TRUE ^^ { case _ => BoolLit(true) } | FALSE ^^ { case _ => BoolLit(false) }

  private lazy val exprR  = (FADD | FSUB | ADD | SUB) ~ term ^^ { case op ~ f => (Op(op), f) }
  private lazy val termR  = (FMUL | FDIV | MUL | DIV) ~ fact ^^ { case op ~ f => (Op(op), f) }

  private def makeBinExpr(le: Expr , re: List[(Op, Expr)]) = {
    re.foldLeft(le) { case (a, (op, e)) => Prim(op, a, e) }
  }
}
