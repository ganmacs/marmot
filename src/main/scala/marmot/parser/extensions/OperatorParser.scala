package marmot.parser.extensions

import marmot._

/*
 * This class defines new operators and expands inner basic paser by them.
 * If this paser fail to parse, it will change this parser to basic paser which is expanded.
 */
class OperatorParser extends ExpandableParser with OperatorToken {
  private val _tmp = expr
  expr = opVar | _tmp
  xParser = Some(new ExpandableParser)

  private lazy val defexpr: PackratParser[Prog] = (expr).* ^^ { case e => Prog(e) }
  private lazy val stmnt: PackratParser[Expr] = defop | define

  override def prog: PackratParser[Prog] =
    (stmnt | (expr <~ EOL)).* ^^ { case e => Prog(e) }

  private def defop: PackratParser[Expr] =
    (DEFOP ~> LPAREN ~> defexpr <~ RPAREN) ~ (LBR ~> expr <~ RBR) ^^ {
      case syntax ~ body => xParser.get.expandWith(syntax.v, body); Empty()
    }

  private def define: PackratParser[Expr] =
    (DEFINE ~> LPAREN ~> defexpr <~ RPAREN) ~ (LBR ~> expr <~ RBR) ^^ {
      case syntax ~ body => expandWith(syntax.v, body); Empty()
    }

  private lazy val opVar: PackratParser[Expr] = COMMA ~> id ^^ { case t => OperatorVar(t) }
}

trait OperatorToken {
  val DEFOP = "defop"
  val DEFINE = "defi"
}
