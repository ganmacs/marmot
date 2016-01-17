package marmot.parser.extensions

import marmot._

class BasicParser extends BaseParser {
  type Pe = PackratParser[Expr]
  type Pp = PackratParser[Prog]

  protected lazy val int: Pe = INT ^^ { case e => IntLit(e.toInt) }
  protected lazy val double: Pe = DOUBLE ^^ { case e => DoubleLit(e.toDouble) }
  protected lazy val id: PackratParser[VarLit] = ID ^^ { case e => VarLit(e) }
  protected lazy val bool: Pe = TRUE ^^ { case _ => BoolLit(true) } | FALSE ^^ { case _ => BoolLit(false) }

  protected lazy val EOL = "\n" | "\r\n" | "\r" | SCOLON
  protected lazy val args = id.*

  protected lazy val exprR  = (FADD | FSUB | ADD | SUB) ~ term ^^ { case op ~ f => (Op(op), f) }
  protected lazy val termR  = (FMUL | FDIV | MUL | DIV) ~ fact ^^ { case op ~ f => (Op(op), f) }

  protected def let: PackratParser[Let] = (LET ~> id) ~ (EQ ~> expr) ~ (IN ~> expr) ^^ {
    case id ~ value ~ body => Let(id, value, body)
  }

  protected def ifexp: PackratParser[IfExp] = (IF ~> expr) ~ (THEN ~> expr) ~ (ELSE ~> expr) ^^ {
    case c ~ e1 ~ e2 => IfExp(c, e1, e2)
  }

  protected def app: Pe = id ~ (LPAREN ~> expr.* <~ RPAREN) ^^ {
    case n ~ exprs => App(n, exprs)
  }

  protected def fun: Pe = (FUN ~> args) ~ (RARROW ~> expr) ^^ {
    case s ~ e => Fun(s, e)
  }

  lazy val term: Pe = fact ~ termR.* ^^ { case l ~ r => makeBinExpr(l, r) }

  def fact: Pe = bool | double | int | id | LPAREN ~> expr <~ RPAREN

  protected def makeBinExpr(le: Expr , re: List[(Op, Expr)]) = {
    re.foldLeft(le) { case (a, (op, e)) => Prim(op, a, e) }
  }

  def prog: Pp = (expr <~ EOL).* ^^ { case e => Prog(e) }

  var expr: Pe = fun | ifexp | let | app | term ~ exprR.* ^^ {
    case l ~ r => makeBinExpr(l, r)
  }

  def parse(in: String): Either[String, Prog] = {
    parseAll(prog, in) match {
      case Success(d, next) => Right(d)
      case NoSuccess(errorMsg, next) =>
        Left(s"$errorMsg : in ${next.pos.line} at column ${next.pos.column}")
    }
  }
}
