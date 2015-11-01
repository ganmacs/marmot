package marmot.parser

import marmot.parser.extensions._

class Parser {
  private lazy val basicParser = new BasicParser
  private lazy val operaotrParser = new OperatorParser

  def parse(in: String) = basicParser.parse(in)
  def parseWithOperator(in: String) = operaotrParser.parse(in)
}
