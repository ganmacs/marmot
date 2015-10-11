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

    describe("fun") {
      assert(evalLine("let a = fun x y -> x + y in a (1 2)") == IntValue(3))
      assert(evalLine("let x = 1 in let f = fun y -> x + y in f (10)") == IntValue(11))
      assert(evalLine("let x = 1 in let f = fun y -> (fun z -> z + x + y) in let k = f (10) in k (20)") == IntValue(31))
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
