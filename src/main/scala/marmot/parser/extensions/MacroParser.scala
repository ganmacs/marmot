package marmot.parser.extensions

import marmot._

class MacroParser(val targetParser: ExpandableParser) extends BaseParser with MacroToken {
  def parse(in: String): Either[String, Prog] = parseAll(stmnt, in) match {
    case Success(d, next) => Right(d)
    case NoSuccess(errorMsg, next) =>
      Left(s"$errorMsg : in ${next.pos.line} at column ${next.pos.column}")
  }

  private lazy val tx: PackratParser[NoTermToken] =
    (TEXPR | TTERM | TFACT) ^^ { NoTermToken(_) }

  private lazy val defmacro: PackratParser[Expr] =
    (MACRO ~> LB ~> tx <~ RB) ~ (LPAREN ~> stmnt <~ RPAREN) ~ (LBR ~> mExpr <~ RBR) ^^ {
      case NoTermToken("$EXPR") ~ s ~ b => this.targetParser.expandWith(s.v, b); Empty()
      case NoTermToken("$TERM") ~ s ~ b => this.targetParser.expandWith(s.v, b); Empty()
      case NoTermToken("$FACT") ~ s ~ b => this.targetParser.expandWith(s.v, b); Empty()
      case t ~ _ ~ _ => throw new Exception(s"Unknown target token ${t.v}")
    }

  private lazy val stmnt: PackratParser[Prog] = (mExpr).* ^^ { Prog(_) }

  private lazy val mExpr: PackratParser[Expr] =
    macroVar | defmacro | mFun | mIf | mLet | mApp | mTerm ~ exprR.* ^^ { case l ~ r => makeBinExpr(l, r) }

  private lazy val macroVar: PackratParser[Expr] = (COMMA ~> id) ~ (SEMI ~> tx) ^^ {
    case e ~ t => MacroVar(e, t)
  }

  private lazy val mLet: PackratParser[Let] = (LET ~> id) ~ (EQ ~> mExpr) ~ (IN ~> mExpr) ^^ {
    case id ~ value ~ body => Let(id, value, body)
  }

  private lazy val mIf: PackratParser[IfExp] = (IF ~> mExpr) ~ (THEN ~> mExpr) ~ (ELSE ~> mExpr) ^^ {
    case c ~ e1 ~ e2 => IfExp(c, e1, e2)
  }

  private lazy val mTerm: PackratParser[Expr] =
    mFact ~ termR.* ^^ { case l ~ r => makeBinExpr(l, r) }

  private lazy val mApp: PackratParser[Expr] = id ~ (LPAREN ~> mExpr.* <~ RPAREN) ^^ {
    case n ~ exprs => App(n, exprs)
  }

  private lazy val mFun: PackratParser[Expr] = (FUN ~> args) ~ (RARROW ~> mExpr) ^^ {
    case s ~ e => Fun(s, e)
  }

  private lazy val args = id.* // FIX?

  private lazy val mFact: PackratParser[Expr] = tx | bool | double | int | id | LPAREN ~> mExpr <~ RPAREN | idx

  private lazy val int = INT ^^ { case e => IntLit(e.toInt) }
  private lazy val double = DOUBLE ^^ { case e => DoubleLit(e.toDouble) }
  private lazy val id = ID ^^ { VarLit(_) }
  private lazy val idx = (IDX | IDX2) ^^ { case e => VarLit(e) }
  private lazy val bool = TRUE ^^ { case _ => BoolLit(true) } | FALSE ^^ { case _ => BoolLit(false) }

  private lazy val exprR  = (FADD | FSUB | ADD | SUB) ~ mTerm ^^ { case op ~ f => (Op(op), f) }
  private lazy val termR  = (FMUL | FDIV | MUL | DIV) ~ mFact ^^ { case op ~ f => (Op(op), f) }

  private def makeBinExpr(le: Expr , re: List[(Op, Expr)]) = {
    re.foldLeft(le) { case (a, (op, e)) => Prim(op, a, e) }
  }
}

trait MacroToken {
  val IDX = "\\"
  val IDX2 = "->"
  val MACRO = "defmacro"
  val TEXPR = "$EXPR"
  val TTERM = "$TERM"
  val TFACT = "$FACT"
}
