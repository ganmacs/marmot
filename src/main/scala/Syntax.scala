case class Op(v: String) {
  override def toString() = v
}

sealed trait Expr

case class IntLit(v: Int) extends Expr {
  override def toString() = v.toString
}

case class DoubleLit(v: Double) extends Expr {
  override def toString() = v.toString
}

case class BoolLit(v: Boolean) extends Expr {
  override def toString() = if (v) "True" else "False"
}

case class VarLit(v: String) extends Expr {
}

case class Prim(op: Op, e1: Expr, e2: Expr) extends Expr {
  override def toString() = s"($op $e1 $e2)"
}

case class IfExp(cond: Expr, e1: Expr, e2: Expr) extends Expr {
  override def toString() = s"(if $cond $e1 $e2)"
}

case class Let(id: VarLit, value: Expr, body: Expr) extends Expr {
  override def toString() = s"(let (($id $value)) $body)"
}

case class Prog(v: List[Expr]) extends Expr {
  override def toString() = v.foldLeft("") { case (a, e) => s"""$a\n$e""" }
}
