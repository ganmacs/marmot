package marmot.parser.extensions

import marmot._
import marmot.parser.Tokens
import util.parsing.combinator.{RegexParsers, PackratParsers}

// Base name is named for using Expandable tarit
abstract class BaseParser extends RegexParsers with PackratParsers with Tokens {
  def parse(in: String): Either[String, Prog]
}
