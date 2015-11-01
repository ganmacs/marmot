package marmot.parser.extensions

import marmot._

/*
 * This class defines new operators and expands inner basic paser by them.
 *  If this paser fail to parse, it will change this parser to basic paser which is expanded.
 */
class OperatorParser(val targetParser: ExpandableParser) extends ExpandableParser with OperatorToken {
  private val xParser: BasicParser = new BasicParser

  expr = define | opVar | defop | fun | ifexp | let | app | term ~ exprR.* ^^ { case l ~ r => makeBinExpr(l, r) }

  private def defop: PackratParser[Expr] =
    (DEFOP ~> LPAREN ~> stmnt <~ RPAREN) ~ (LBR ~> expr <~ RBR) ^^ {
      case syntax ~ body => this.targetParser.expandWith(syntax.v, body); Empty()
    }

  private def define: PackratParser[Expr] =
    (DEFINE ~> LPAREN ~> stmnt <~ RPAREN) ~ (LBR ~> expr <~ RBR) ^^ {
      case syntax ~ body => { expandWith(syntax.v, body); Empty() }
    }

  private lazy val opVar: PackratParser[Expr] = COMMA ~> id ^^ { case t => OperatorVar(t) }
}

trait OperatorToken {
  val DEFOP = "defop"
  val DEFINE = "defi"
}
