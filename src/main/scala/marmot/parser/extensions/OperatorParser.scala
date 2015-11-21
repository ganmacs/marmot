package marmot.parser.extensions

import marmot._

/*
 * This class defines new operators and expands inner basic paser by them.
 * If this paser fail to parse, it will change this parser to basic paser which is expanded.
 */
class OperatorParser extends ExpandableParser with OperatorToken {
  private val _tmp = expr
  expr = opVar | _tmp

  private lazy val defexpr: PackratParser[Prog] = (expr).* ^^ { case e => Prog(e) }
  private lazy val stmnt: PackratParser[Expr] = defop | define

  override def prog: PackratParser[Prog] =
    (stmnt | (expr <~ EOL)).* ^^ { case e => Prog(e) }

  private def namespace: PackratParser[Expr] = (LB ~> NAMESPACE <~ RB) ^^ { v => Namespace(v) }

  private def defop: PackratParser[Expr] =
    (DEFOP ~> namespace) ~ (LPAREN ~> defexpr <~ RPAREN)  ~ (LBR ~> expr <~ RBR) ^^ {
      case Namespace(v) ~ syntax ~ body => xParsers.get(v) match {
        case None => Empty()
        case Some(k) => k.expandWith(syntax.v, body); Empty()
      }
    }

  private def define: PackratParser[Expr] =
    (DEFINE ~> namespace) ~ (LPAREN ~> defexpr <~ RPAREN)  ~ (LBR ~> expr <~ RBR) ^^ {
      case Namespace(v) ~ syntax ~ body =>
        this._namespace = v
        expandWith(syntax.v, body)
        this._namespace = "EMPTY"
        Empty()
    }

  private lazy val opVar: PackratParser[Expr] = COMMA ~> id ^^ { case t => OperatorVar(t) }
}

trait OperatorToken {
  val DEFOP = "defop"
  val DEFINE = "defi"
}
