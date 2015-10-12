package marmot.parser

import marmot.parser.extensions._

class Parser {
  private lazy val basicParser = new BasicParser
  private lazy val macroParser = new MacroParser(basicParser)
  private lazy val parsers: List[BaseParser] = List(basicParser, macroParser)

  def parse(in: String, i: Int = 0) = parser(i).parse(in)

  private def parser(i: Int) = parsers.apply(i)
}
