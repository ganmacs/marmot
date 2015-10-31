package marmot.parser

import org.scalatest.FunSpec
import marmot._
import marmot.parser._
import scala.io.Source

class ParserTest extends FunSpec {
  describe ("Basic Parser") {
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

    describe("Prim return Prim obj") {
      assert(parseLine("1 + 2") == Prim(Op("+"), IntLit(1), IntLit(2)))
      assert(parseLine("1.2 +. 2.0") == Prim(Op("+."), DoubleLit(1.2), DoubleLit(2.0)))
    }

    describe("if return Prim obj") {
      assert(parseLine("if true then 2 else 1") == IfExp(BoolLit(true), IntLit(2), IntLit(1)))
    }

    describe("let return Prim obj") {
      assert(parseLine("let x = 1 in x") == Let(VarLit("x"), IntLit(1), VarLit("x")))
    }

    describe("fun return Fun obj") {
      assert(parseLine("fun x y -> x + y") ==
        Fun(List(VarLit("x"), VarLit("y")),
          Prim(Op("+"), VarLit("x"), VarLit("y"))))
    }

    describe("app return App obj") {
      assert(parseLine("f (1 2)") ==
        App(VarLit("f"), List(IntLit(1), IntLit(2))))
    }
  }

  describe("Operator definition") {
    describe("operator Parser") {
      val parser = new Parser
      val lines  = readfile("src/test/resouces/operator")

      parser.parse(lines(0), 2) match {
        case Right(Prog(x)) => { assert( x(0) == Empty()) }
        case Left(x) => throw new Exception(x)
      }
      parser.parse(lines(1), 0) match {
        case Right(Prog(x)) => {
          println(x(0))
          assert(x(0) == IfExp(BoolLit(true), IntLit(8), IntLit(9)))
        }
        case Left(x) => throw new Exception(x)
      }
    }
  }

  describe("Macro definition") {
    describe("Macro Parser") {
      val parser = new Parser
      val lines  = readfile("src/test/resouces/macro")

      parser.parse(lines(0), 1) match {
        case Right(Prog(x)) => { assert( x(0) == Empty()) }
        case Left(x) => throw new Exception(x)
      }
      parser.parse(lines(1), 0) match {
        case Right(Prog(x)) => {
          assert(x(0) == IfExp(BoolLit(true), IntLit(8), IntLit(9)))
        }
        case Left(x) => throw new Exception(x)
      }
    }

    describe ("tri") {
      val parser = new Parser
      val lines  = readfile("src/test/resouces/tri")

      parser.parse(lines(0), 1) match {
        case Right(Prog(x)) => { assert( x(0) == Empty()) }
        case Left(x) => throw new Exception(x)
      }
      val y = parser.parse(lines(1), 0) match {
        case Right(Prog(x)) => {
          assert(x(0) ==
            Prim(Op("*"), IntLit(2),
              Prim(Op("+"),
                Prim(Op("+"), IntLit(1), IntLit(2)),
                IntLit(4))
            )
          )
        }
        case Left(x) => throw new Exception(x)
      }
    }

    describe ("lambda") {
      val parser = new Parser
      val lines  = readfile("src/test/resouces/lambda")

      parser.parse(lines(0), 1) match {
        case Right(Prog(x)) => { assert( x(0) == Empty()) }
        case Left(x) => throw new Exception(x)
      }
      parser.parse(lines(1), 0) match {
        case Right(Prog(x)) => {
          assert(x(0) ==
            Let(
              VarLit("x"),
              Fun(List(VarLit("x")), Prim(Op("+"),VarLit("x"),IntLit(1))),
              App(VarLit("x"),List(IntLit(4))))
          )
        }
        case Left(x) => throw new Exception(x)
      }
    }
  }

  def readfile(filename: String) = {
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    lines
  }

  def parseLine(in: String, i: Int = 0) = {
    val parser = new Parser
    parser.parse(in, i) match {
      case Right(Prog(x)) => x(0)
      case Left(x) => throw new Exception(x)
    }
  }
}
