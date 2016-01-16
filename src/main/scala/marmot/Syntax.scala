package marmot

case class Op(v: String) {
  override def toString() = v
}

sealed trait Expr
case class Empty() extends Expr
case class IntLit(v: Int) extends Expr
case class DoubleLit(v: Double) extends Expr
case class BoolLit(v: Boolean) extends Expr
case class VarLit(v: String) extends Expr
case class Fun(args: List[VarLit], body: Expr) extends Expr
case class App(name: VarLit, body: List[Expr]) extends Expr
case class Prim(op: Op, e1: Expr, e2: Expr) extends Expr
case class IfExp(cond: Expr, e1: Expr, e2: Expr) extends Expr
case class Let(id: VarLit, value: Expr, body: Expr) extends Expr
case class Prog(v: List[Expr]) extends Expr
case class OperatorVar(varname: VarLit) extends Expr
case class VarWithContext(varname: VarLit, context: String) extends Expr
case class Context(v: String) extends Expr
