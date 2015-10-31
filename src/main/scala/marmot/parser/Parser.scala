package marmot.parser

import marmot.parser.extensions._

class Parser {
  private lazy val basicParser = new BasicParser
  private lazy val macroParser = new MacroParser(basicParser)
  private lazy val operaotrParser = new OperatorParser(basicParser)
  private lazy val parsers: List[BaseParser] = List(basicParser, macroParser, operaotrParser)

  def parse(in: String, i: Int = 0) = parsers(i).parse(in)

  def parseA(in: String) = macroParser.parse(in) match {
    case Left(x) => x
    case Right(x) => x
  }
}
