package marmot.parser.extensions

import marmot._


class OperatorParser extends ExpandableParser with OperatorToken {
  private val _tmp = expr
  expr = opVar | _tmp

  private lazy val stmnt: Pe = defi | defop

  private lazy val defexpr: Pp = (expr).* ^^ { case e => Prog(e) }

  override def prog: Pp = (stmnt | (expr <~ EOL)).* ^^ { case e => Prog(e) }

  private def context: Pe = (LB ~> CONTEXT <~ RB) ^^ { v => Namespace(v) }

  private def defop: Pe =
    (DEFOP ~> context) ~ (LPAREN ~> defexpr <~ RPAREN)  ~ (LBR ~> expr <~ RBR) ^^ {
      case Namespace(v) ~ syntax ~ body => parserMap.get(v) match {
        case None => Empty()
        case Some(p) => p.expandSyntax(syntax.v, body); Empty()
      }
    }

  private def defi: Pe =
    (DEFINE ~> context) ~ (LPAREN ~> defexpr <~ RPAREN)  ~ (LBR ~> expr <~ RBR) ^^ {
      case Namespace(v) ~ syntax ~ body =>
        doInNS(v, { expandSyntax(syntax.v, body) })
        Empty()
    }

  private lazy val opVar: Pe = COMMA ~> id ^^ { case t => OperatorVar(t) }
  // private lazy val dargs: PackratParser[Prog] = (expr).* ^^ { case e => Prog(e) }
  // private lazy val varWithContext: PackratParser[Expr] = id ~ (COLON ~> CONTEXT) ^^ { case t ~ c => VarWithContext(t, c) }
}

trait OperatorToken {
  val DEFOP = "defop"
  val DEFINE = "defi"
}
