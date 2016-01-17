package marmot.parser.extensions

import marmot._
import marmot.parser.Tokens
import scala.util.parsing.combinator.{RegexParsers, PackratParsers}

abstract class BaseParser extends RegexParsers with PackratParsers with Tokens {
  def parse(in: String): Either[String, Prog]
}
