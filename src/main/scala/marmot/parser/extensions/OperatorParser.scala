package marmot.parser.extensions

import marmot._

class OperatorParser extends ExpandableParser with OperatorToken {

  private lazy val stmnt: Pe = defi | defop

  private lazy val str = """[^):\s]+""".r ^^ { case t => VarLit(t) }
  private lazy val opVar: Pe = str ~ context ^^ { case s ~ c => VarWithContext(s, c) }
  private lazy val defexpr: Pp = (opVar | str).* ^^ { case e => Prog(e) }

  private lazy val varWithContext: PackratParser[VarWithContext] =
    id ~ context ^^ { case t ~ c => VarWithContext(t, c) }

  override def prog: Pp = (stmnt | (expr <~ EOL)).* ^^ { case e => Prog(e) }

  private def context: PackratParser[Context] = (COLON ~> CONTEXT) ^^ { v => Context(v) }

  private lazy val body: Pe = LBR ~> expr <~ RBR
  private lazy val defArgs: Pp = LPAREN ~> defexpr <~ RPAREN

  private def defop: Pe = DEFOP ~> defArgs ~ context ~ body ^^ {
    case  syntax ~ Context(v) ~ body => parserMap.get(v) match {
      case None => Empty()
      case Some(p) => p.expandSyntax(syntax.v, body); Empty()
    }
  }

  private def defi: Pe = DEFINE ~> defArgs ~ context ~ body ^^ {
    case syntax ~ Context(v) ~ body => expandSyntax(syntax.v, body); Empty()
  }
}

trait OperatorToken {
  val DEFOP = "defop"
  val DEFINE = "defi"
}
