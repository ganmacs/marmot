package marmot.parser

trait Tokens {
  val INT = """(0|[1-9][0-9]*)""".r
  val DOUBLE =  """(0|[1-9][0-9]*)\.[0-9]+""".r
  val ID = """[a-z][a-zA-Z0-9]*""".r

  val COMMA = ","
  val SEMI = ":"
  val LPAREN = "("
  val RPAREN = ")"
  val LB = "["
  val RB = "]"
  val LBR = "{"
  val RBR = "}"
  val EQ = "="
  val LP = "{"
  val RP = "}"
  val ADD = "+"
  val SUB = "-"
  val MUL = "*"
  val DIV = "/"
  val FADD = "+."
  val FSUB = "-."
  val FMUL = "*."
  val FDIV = "/."

  val IF = "if"
  val THEN = "then"
  val ELSE = "else"
  val LET = "let"
  val IN = "in"
  val FUN = "fun"
  val RARROW = "->"

  val TRUE = "true"
  val FALSE = "false"
}
