package marmot.parser.extensions

import marmot._

class OperatorParser(val targetParser: BasicParser) extends BaseParser with OperatorToken {
  def parse(in: String): Either[String, Prog] = parseAll(stmnt, in) match {
    case Success(d, next) => Right(d)
    case NoSuccess(errorMsg, next) =>
      Left(s"$errorMsg : in ${next.pos.line} at column ${next.pos.column}")
  }
  private val xParser: BasicParser = new BasicParser

  private lazy val stmnt: PackratParser[Prog] =  (oExpr).* ^^ { Prog(_) }

  private lazy val oExpr: PackratParser[Expr] =
    opVar | defop | oFun | oIf | oLet | oApp | oTerm ~ exprR.* ^^ { case l ~ r => makeBinExpr(l, r) }

  private lazy val defop: PackratParser[Expr] =
    (DEFOP ~> LPAREN ~> stmnt <~ RPAREN) ~ (LBR ~> oExpr <~ RBR) ^^ {
      case syntax ~ body => this.targetParser.registerOperator(syntax.v, body); Empty()
    }

  private lazy val define: PackratParser[Expr] =
    (DEFINE ~> LPAREN ~> stmnt <~ RPAREN) ~ (LBR ~> oExpr <~ RBR) ^^ {
      case syntax ~ body => {
        // parser must be dictory to coexist some parsers?
        xParser.buildParser(syntax.v, body);
        Empty()
      }
    }

  private lazy val opVar: PackratParser[Expr] = COMMA ~> id ^^ { case t => OperatorVar(t) }

  private lazy val oLet: PackratParser[Let] = (LET ~> id) ~ (EQ ~> oExpr) ~ (IN ~> oExpr) ^^ {
    case id ~ value ~ body => Let(id, value, body)
  }

  private lazy val oIf: PackratParser[IfExp] = (IF ~> oExpr) ~ (THEN ~> oExpr) ~ (ELSE ~> oExpr) ^^ {
    case c ~ e1 ~ e2 => IfExp(c, e1, e2)
  }

  private lazy val oTerm: PackratParser[Expr] =
    oFact ~ termR.* ^^ { case l ~ r => makeBinExpr(l, r) }

  private lazy val oApp: PackratParser[Expr] =
    id ~ (LPAREN ~> oExpr.* <~ RPAREN) ^^ { case n ~ exprs => App(n, exprs) }

  private lazy val oFun: PackratParser[Expr] = (FUN ~> args) ~ (RARROW ~> oExpr) ^^ {
    case s ~ e => Fun(s, e)
  }

  private lazy val args = id.* // FIX?

  private lazy val oFact: PackratParser[Expr] = bool | double | int | id | LPAREN ~> oExpr <~ RPAREN

  private lazy val int = INT ^^ { case e => IntLit(e.toInt) }
  private lazy val double = DOUBLE ^^ { case e => DoubleLit(e.toDouble) }
  private lazy val id = ID ^^ { VarLit(_) }
  private lazy val bool =
    TRUE ^^ { case _ => BoolLit(true) } |
  FALSE ^^ { case _ => BoolLit(false) }

  private lazy val exprR  = (FADD | FSUB | ADD | SUB) ~ oTerm ^^ { case op ~ f => (Op(op), f) }
  private lazy val termR  = (FMUL | FDIV | MUL | DIV) ~ oFact ^^ { case op ~ f => (Op(op), f) }

  private def makeBinExpr(le: Expr , re: List[(Op, Expr)]) = {
    re.foldLeft(le) { case (a, (op, e)) => Prim(op, a, e) }
  }
}

trait OperatorToken {
  val DEFOP = "defop"
  val DEFINE = "define"
}
