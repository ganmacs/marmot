package marmot.parser

import marmot.parser.extensions._

class Parser {
  private lazy val basicParser = new BasicParser
  private lazy val operatorParser = new OperatorParser

  def parse(in: String) = basicParser.parse(in)
  def parseWithOperator(in: String) = operatorParser.parse(in)
}
