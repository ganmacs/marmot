package marmot.parser.extensions

import marmot._

class BasicParser extends BaseParser {
  def parse(in: String): Either[String, Prog] = parseAll(stmnt, in) match {
    case Success(d, next) => Right(d)
    case NoSuccess(errorMsg, next) =>
      Left(s"$errorMsg : in ${next.pos.line} at column ${next.pos.column}")
  }

  lazy val int: PackratParser[Expr] = INT ^^ { case e => IntLit(e.toInt) }
  lazy val double: PackratParser[Expr] = DOUBLE ^^ { case e => DoubleLit(e.toDouble) }
  lazy val id: PackratParser[VarLit] = ID ^^ { case e => VarLit(e) }
  lazy val bool: PackratParser[Expr] = TRUE ^^ { case _ => BoolLit(true) } | FALSE ^^ { case _ => BoolLit(false) }

  protected lazy val exprR  = (FADD | FSUB | ADD | SUB) ~ term ^^ { case op ~ f => (Op(op), f) }
  protected lazy val termR  = (FMUL | FDIV | MUL | DIV) ~ fact ^^ { case op ~ f => (Op(op), f) }

  lazy val term: PackratParser[Expr] =
    fact ~ termR.* ^^ { case l ~ r => makeBinExpr(l, r) }

  var expr: PackratParser[Expr] =
    fun | ifexp | let | app | term ~ exprR.* ^^ { case l ~ r => makeBinExpr(l, r) }

  protected lazy val args = id.* // FIX?

  def stmnt: PackratParser[Prog] = (expr).* ^^ { case e => Prog(e) }

  protected def let: PackratParser[Let] = (LET ~> id) ~ (EQ ~> expr) ~ (IN ~> expr) ^^ {
    case id ~ value ~ body => Let(id, value, body)
  }

  protected def ifexp: PackratParser[IfExp] = (IF ~> expr) ~ (THEN ~> expr) ~ (ELSE ~> expr) ^^ {
    case c ~ e1 ~ e2 => IfExp(c, e1, e2)
  }

  protected def app: PackratParser[Expr] = id ~ (LPAREN ~> expr.* <~ RPAREN) ^^ {
    case n ~ exprs => App(n, exprs)
  }

  protected def fun: PackratParser[Expr] = (FUN ~> args) ~ (RARROW ~> expr) ^^ {
    case s ~ e => Fun(s, e)
  }

  def fact: PackratParser[Expr] = bool | double | int | id | LPAREN ~> expr <~ RPAREN

  protected def makeBinExpr(le: Expr , re: List[(Op, Expr)]) = {
    re.foldLeft(le) { case (a, (op, e)) => Prim(op, a, e) }
  }
}
