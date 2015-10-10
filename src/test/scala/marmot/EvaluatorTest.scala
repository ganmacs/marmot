package marmot

import org.scalatest.FunSpec

class EvaluatorTest extends FunSpec {
  describe("One Line")  {
    describe("literal") {
      assert(evalLine("1") == IntValue(1))
      assert(evalLine("1.2") == DoubleValue(1.2))
      assert(evalLine("true") == BoolValue(true))
    }

    describe("Prim") {
      assert(evalLine("1 + 1") == IntValue(2))
      assert(evalLine("1.2 +. 1.3") == DoubleValue(2.5))
    }

    describe("if") {
      assert(evalLine("if true then 1 else 2") == IntValue(1))
      assert(evalLine("if false then 1 else 2") == IntValue(2))
    }

    describe("let") {
      assert(evalLine("let x = 1 in x") == IntValue(1))

      it ("has scope") {
        assert(evalLine("let x = 1 in x + (let x = 10 in x + x) ") == IntValue(21))
      }
    }
  }

  def evalLine(in: String): Value = {
    Parser.parse(in) match {
      case Right(Prog(x)) => Evaluator.evalAll(x.apply(0), Env.empty).apply(0)
      case Left(x) => throw new Exception(s"""parse error $x\n""")
    }
  }

  def eval(in: String): List[Value] = {
    Parser.parse(in) match {
      case Right(Prog(x)) => Evaluator.evalAll(x.apply(0), Env.empty)
      case Left(x) => throw new Exception(s"""parse error $x\n""")
    }
  }
}
