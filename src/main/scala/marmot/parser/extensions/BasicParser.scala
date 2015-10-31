package marmot.parser.extensions

import marmot._

class BasicParser extends Expandable {
  def parse(in: String): Either[String, Prog] = parseAll(stmnt, in) match {
    case Success(d, next) => Right(d)
    case NoSuccess(errorMsg, next) =>
      Left(s"$errorMsg : in ${next.pos.line} at column ${next.pos.column}")
  }

  def registerOperator(exprs: List[Expr], semantics: Expr): Unit = {
    expand(exprs, semantics)
  }

  def registerRule(t: NoTermToken, exprs: List[Expr], semantics: Expr): Unit = t match {
    case NoTermToken("$EXPR") => expand(exprs, semantics)
    case NoTermToken("$TERM") => expand(exprs, semantics)
    case NoTermToken("$FACT") => expand(exprs, semantics)
    case _ => throw new Exception(s"Unknown target token ${t.v}")
  }

  private lazy val stmnt: PackratParser[Prog] = (expr).* ^^ { case e => Prog(e) }

  override var expr: PackratParser[Expr] =
    fun | ifexp | let | app | term ~ exprR.* ^^ { case l ~ r => makeBinExpr(l, r) }

  lazy val let: PackratParser[Let] = (LET ~> id) ~ (EQ ~> expr) ~ (IN ~> expr) ^^ {
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

  lazy override val int: PackratParser[Expr] = INT ^^ { case e => IntLit(e.toInt) }
  lazy override val double: PackratParser[Expr] = DOUBLE ^^ { case e => DoubleLit(e.toDouble) }
  lazy override val id: PackratParser[VarLit] = ID ^^ { case e => VarLit(e) }
  lazy override val bool: PackratParser[Expr] = TRUE ^^ { case _ => BoolLit(true) } | FALSE ^^ { case _ => BoolLit(false) }

  private lazy val exprR  = (FADD | FSUB | ADD | SUB) ~ term ^^ { case op ~ f => (Op(op), f) }
  private lazy val termR  = (FMUL | FDIV | MUL | DIV) ~ fact ^^ { case op ~ f => (Op(op), f) }

  private def makeBinExpr(le: Expr , re: List[(Op, Expr)]) = {
    re.foldLeft(le) { case (a, (op, e)) => Prim(op, a, e) }
  }
}
