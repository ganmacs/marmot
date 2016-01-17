package marmot

sealed trait Value
case class IntValue(v: Int) extends Value
case class DoubleValue(v: Double) extends Value
case class BoolValue(v: Boolean) extends Value
case class ArrayValue(v: List[Value]) extends Value
case class FunValue(args: List[VarLit], body: Expr, env: Env[Value]) extends Value
