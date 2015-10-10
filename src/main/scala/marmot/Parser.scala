package marmot

import util.parsing.combinator.{RegexParsers, PackratParsers}

object Parser extends RegexParsers with PackratParsers with Tokens {
  def parse(in: String) = parseAll(stmnt, in) match {
    case Success(d, next) => Right(d)
    case NoSuccess(errorMsg, next) =>
      Left(s"$errorMsg : in ${next.pos.line} at column ${next.pos.column}")
  }

  private lazy val stmnt: PackratParser[Prog] = (expr).* ^^ { case e => Prog(e) }

  private lazy val expr: PackratParser[Expr] =
    ifexp | let |  term ~ exprR.* ^^ { case l ~ r => makeBinExpr(l, r) }

  private lazy val let: PackratParser[Let] = (LET ~> id) ~ (EQ ~> expr) ~ (IN ~> expr) ^^ {
    case id ~ value ~ body => Let(id, value, body)
  }

  private lazy val ifexp: PackratParser[IfExp] = (IF ~> expr) ~ (THEN ~> expr) ~ (ELSE ~> expr) ^^ {
    case c ~ e1 ~ e2 => IfExp(c, e1, e2)
  }

  private lazy val term: PackratParser[Expr] =
    fact ~ termR.* ^^ { case l ~ r => makeBinExpr(l, r) }

  private lazy val fact: PackratParser[Expr] = bool | double | int | id | LPAREN ~> expr <~ RPAREN

  private lazy val int = INT ^^ { case e => IntLit(e.toInt) }
  private lazy val double = DOUBLE ^^ { case e => DoubleLit(e.toDouble) }
  private lazy val id = ID ^^ { case e => VarLit(e) }
  private lazy val bool = TRUE ^^ { case _ => BoolLit(true) } | FALSE ^^ { case _ => BoolLit(false) }

  private lazy val exprR  = (FADD | FSUB | ADD | SUB) ~ term ^^ { case op ~ f => (Op(op), f) }
  private lazy val termR  = (FMUL | FDIV | MUL | DIV) ~ fact ^^ { case op ~ f => (Op(op), f) }

  private def makeBinExpr(le: Expr , re: List[(Op, Expr)]) = {
    re.foldLeft(le) { case (a, (op, e)) => Prim(op, a, e) }
  }
}
