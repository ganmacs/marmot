package maromot

import org.scalatest.FunSpec

class ParserTest extends FunSpec {
  describe("One Line script") {
    describe("Literal") {
      it ("returns IntLit wrapped value") {
        assert(parseLine("1") == IntLit(1))
        assert(parseLine("100") == IntLit(100))
      }

      it ("returns DoubleLit wrapped value") {
        assert(parseLine("3.14") == DoubleLit(3.14))
        assert(parseLine("0.34") == DoubleLit(0.34))
      }

      it ("returns IdLit wrapped value") {
        assert(parseLine("x") == VarLit("x"))
        assert(parseLine("xS2") == VarLit("xS2"))
      }

      it ("returns BoolLit wrapped value") {
        assert(parseLine("true") == BoolLit(true))
      }
    }

    describe("Prim") {
      it("return Primee") {
        assert(parseLine("1 + 2") == Prim(Op("+"), IntLit(1), IntLit(2)))
        assert(parseLine("1.2 +. 2.0") == Prim(Op("+."), DoubleLit(1.2), DoubleLit(2.0)))
      }
    }

    describe("if") {
      it("return if exp") {
        assert(parseLine("if true then 2 else 1") == IfExp(BoolLit(true), IntLit(2), IntLit(1)))
      }
    }
  }

  def parseLine(in: String) =
    Parser.parse(in) match {
      case Right(Prog(x)) => x.apply(0)
      case Left(x) => throw new Exception(x)
    }
}