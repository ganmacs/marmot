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
    def parseWithOprator(filename: String): Either[String, Prog] = {
      val parser = new Parser
      val buff = readfileAll(filename)
      parser.parseWithOperator(buff)
    }

    describe("defi operator Parser") {
      parseWithOprator("src/test/resouces/operator") match {
        case Right(Prog(x)) => {
          assert(x(0) == Empty())
          assert(x(1) == IfExp(BoolLit(true), IntLit(8), IntLit(9)))
        }
        case Left(x) => throw new Exception(x)
      }
    }

    describe("select Parser") {
      parseWithOprator("src/test/resouces/select") match {
        case Right(Prog(x)) => {
          assert(x(0) == Empty())
          assert(x(1) == Empty())
          assert(x(2) == IfExp(BoolLit(true), IntLit(10), Prim(Op("+"), IntLit(9), IntLit(10))))
        }
        case Left(x) => throw new Exception(x)
      }
    }

    describe("expr_error line") {
      val ret =  parseWithOprator("src/test/resouces/expr_error") match {
        case Right(Prog(x)) => "success"
        case Left(_) => "false"
      }
      assert(ret == "false")
    }

    describe("scope_operator Parser") {
      val ret =  parseWithOprator("src/test/resouces/scope_operator") match {
        case Right(Prog(x)) => "success"
        case Left(_) => "false"
      }
      assert(ret == "false")
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

  def readfileAll(filename: String): String = {
    readfile(filename).mkString("\n")
  }

  def readfile(filename: String) = {
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    lines
  }

  def parseLine(in: String, i: Int = 0) = {
    val parser = new Parser
    parser.parse(in + ";", i) match {
      case Right(Prog(x)) => x(0)
      case Left(x) => throw new Exception(x)
    }
  }
}
