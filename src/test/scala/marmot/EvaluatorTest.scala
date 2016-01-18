package marmot

import org.scalatest.FunSpec
import marmot.parser._

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

    describe("Comp") {
      assert(evalLine("1 == 1") == BoolValue(true))
      assert(evalLine("2 < 1") == BoolValue(false))
      assert(evalLine("2.0 < 1.0") == BoolValue(false))
    }

    describe("if") {
      assert(evalLine("if true then 1 else 2") == IntValue(1))
      assert(evalLine("if false then 1 else 2") == IntValue(2))
      assert(evalLine("if (1 < 0) then 1 else 2") == IntValue(2))
      assert(evalLine("if (1 < 0) then 1 else 2") == IntValue(2))
    }

    describe("Array") {
      assert(evalLine("[1; 2; 3]") == ArrayValue(List(IntValue(1), IntValue(2), IntValue(3))))
      assert(evalLine("let a = [1; 2; 3] in a[0]") == IntValue(1))
      assert(evalLine("let a = [1; 2; 3] in a[1]") == IntValue(2))

      assert(evalLine("1 :: [2; 3]") == ArrayValue(List(IntValue(1), IntValue(2), IntValue(3))))
      assert(evalLine("let a = 1 :: [2; 3] in a[2]") == IntValue(3))
    }

    describe("let") {
      assert(evalLine("let x = 1 in x") == IntValue(1))

      it ("has scope") {
        assert(evalLine("let x = 1 in x + (let x = 10 in x + x) ") == IntValue(21))
      }
    }

    describe("fun") {
      assert(evalLine("let a = fun x -> if x == 0 then 1 else x * a(x - 1) in a(5)") == IntValue(120))
      assert(evalLine("let a = fun x y -> x + y in a (1 2)") == IntValue(3))
      assert(evalLine("let a = fun x y -> x + y in (a (1 2)) + 1") == IntValue(4))
      assert(evalLine("let x = 1 in let f = fun y -> x + y in f (10)") == IntValue(11))
      assert(evalLine("let x = 1 in let f = fun y -> (fun z -> z + x + y) in let k = f (10) in k (20)") == IntValue(31))
    }
  }

  def evalLine(in: String): Value = {
    val parser = new Parser
    parser.parse(in + ";") match {
      case Right(Prog(x)) => Evaluator.evalAll(x(0), Env.empty)(0)
      case Left(x) => throw new Exception(s"""parse error $x\n""")
    }
  }

  def eval(in: String): List[Value] = {
    val parser = new Parser
    parser.parse(in) match {
      case Right(Prog(x)) => Evaluator.evalAll(x(0), Env.empty)
      case Left(x) => throw new Exception(s"""parse error $x\n""")
    }
  }
}
