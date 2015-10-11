package marmot.parser

import marmot.parser.extensions._

class Parser {
  private lazy val basicParser = new BasicParser
  private lazy val parsers: List[BaseParser] = List(basicParser)

  private def parser(i: Int) = parsers.apply(i)

  def parse(in: String) = parser(0).parse(in)
}
