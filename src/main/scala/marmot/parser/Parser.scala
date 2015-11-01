package marmot.parser

import marmot.parser.extensions._

class Parser {
  private lazy val expandableParser = new ExpandableParser
  private lazy val macroParser = new MacroParser(expandableParser)
  private lazy val operaotrParser = new OperatorParser(expandableParser)
  private lazy val parsers: List[BaseParser] = List(expandableParser, macroParser, operaotrParser)

  def parse(in: String, i: Int = 0) = parsers(i).parse(in)

  def parseA(in: String) = macroParser.parse(in) match {
    case Left(x) => x
    case Right(x) => x
  }
}
