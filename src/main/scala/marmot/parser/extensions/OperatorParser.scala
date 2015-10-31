package marmot.parser.extensions

import marmot._

/*
 * This class defines new operators and expands inner basic paser by them.
 *  If this paser fail to parse, it will change this parser to basic paser which is expanded.
 */
class OperatorParser(val targetParser: BasicParser) extends Expandable with OperatorToken {
  def parse(in: String): Either[String, Prog] = parseAll(stmnt, in) match {
    case Success(d, next) => Right(d)
    case NoSuccess(errorMsg, next) =>
      Left(s"$errorMsg : in ${next.pos.line} at column ${next.pos.column}")
  }
  private val xParser: BasicParser = new BasicParser

  private lazy val stmnt: PackratParser[Prog] =  (expr).* ^^ { Prog(_) }

  var expr: PackratParser[Expr] =
    opVar | defop | oFun | oIf | oLet | oApp | term ~ exprR.* ^^ { case l ~ r => makeBinExpr(l, r) }

  private lazy val defop: PackratParser[Expr] =
    (DEFOP ~> LPAREN ~> stmnt <~ RPAREN) ~ (LBR ~> expr <~ RBR) ^^ {
      case syntax ~ body => this.targetParser.registerOperator(syntax.v, body); Empty()
    }

  private lazy val define: PackratParser[Expr] =
    (DEFINE ~> LPAREN ~> stmnt <~ RPAREN) ~ (LBR ~> expr <~ RBR) ^^ {
      case syntax ~ body => {
        xParser.buildParser(syntax.v, body);
        Empty()
      }
    }

  private lazy val opVar: PackratParser[Expr] = COMMA ~> id ^^ { case t => OperatorVar(t) }

  private lazy val oLet: PackratParser[Let] = (LET ~> id) ~ (EQ ~> expr) ~ (IN ~> expr) ^^ {
    case id ~ value ~ body => Let(id, value, body)
  }

  private lazy val oIf: PackratParser[IfExp] = (IF ~> expr) ~ (THEN ~> expr) ~ (ELSE ~> expr) ^^ {
    case c ~ e1 ~ e2 => IfExp(c, e1, e2)
  }

  lazy val term: PackratParser[Expr] =
    fact ~ termR.* ^^ { case l ~ r => makeBinExpr(l, r) }

  private lazy val oApp: PackratParser[Expr] =
    id ~ (LPAREN ~> expr.* <~ RPAREN) ^^ { case n ~ exprs => App(n, exprs) }

  private lazy val oFun: PackratParser[Expr] = (FUN ~> args) ~ (RARROW ~> expr) ^^ {
    case s ~ e => Fun(s, e)
  }

  private lazy val args = id.* // FIX?

  lazy val fact: PackratParser[Expr] = bool | double | int | id | LPAREN ~> expr <~ RPAREN

  lazy val int: PackratParser[Expr] = INT ^^ { case e => IntLit(e.toInt) }
  lazy val double: PackratParser[Expr] = DOUBLE ^^ { case e => DoubleLit(e.toDouble) }
  lazy val id: PackratParser[VarLit] = ID ^^ { VarLit(_) }
  lazy val bool: PackratParser[Expr] =
    TRUE ^^ { case _ => BoolLit(true) } | FALSE ^^ { case _ => BoolLit(false) }

  private lazy val exprR  = (FADD | FSUB | ADD | SUB) ~ term ^^ { case op ~ f => (Op(op), f) }
  private lazy val termR  = (FMUL | FDIV | MUL | DIV) ~ fact ^^ { case op ~ f => (Op(op), f) }

  private def makeBinExpr(le: Expr , re: List[(Op, Expr)]) = {
    re.foldLeft(le) { case (a, (op, e)) => Prim(op, a, e) }
  }
}

trait OperatorToken {
  val DEFOP = "defop"
  val DEFINE = "define"
}
