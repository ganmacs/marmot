package marmot

sealed trait Value
case class IntValue(v: Int) extends Value
case class DoubleValue(v: Double) extends Value
case class BoolValue(v: Boolean) extends Value
