package marmot.parser.extensions

import marmot._


class OperatorParser extends ExpandableParser with OperatorToken {
  private val _tmp = expr
  expr = opVar | _tmp

  private lazy val stmnt: Pe = defi | defop

  private lazy val defexpr: Pp = (expr).* ^^ { case e => Prog(e) }

  override def prog: Pp = (stmnt | (expr <~ EOL)).* ^^ { case e => Prog(e) }

  private def ncontext: Pe = (LB ~> CONTEXT <~ RB) ^^ { v => Context(v) }
  private def context: Pe = (COLON ~> CONTEXT) ^^ { v => Context(v) }

  private lazy val body: Pe = LBR ~> expr <~ RBR
  private lazy val defArgs: Pp = LPAREN ~> defexpr <~ RPAREN

  private def defop: Pe = DEFOP ~> defArgs ~ context ~ body ^^ {
      case  syntax ~ Context(v) ~ body => parserMap.get(v) match {
        case None => Empty()
        case Some(p) => p.expandSyntax(syntax.v, body); Empty()
      }
    }

  private def defi: Pe = DEFINE ~> defArgs ~ context ~ body ^^ {
      case syntax ~ Context(v) ~ body =>
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
