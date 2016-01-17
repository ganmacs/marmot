package marmot.parser

import org.scalatest.FunSpec
import marmot._
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

    describe("add") {
      parseWithOprator("src/test/resouces/add") match {
        case Right(Prog(x)) => {
          assert(x(0) == Empty())
          assert(x(1) == IntLit(1000))
        }
        case Left(x) => throw new Exception(x)
      }
    }

    describe("multiple_parser") {
      val ret =  parseWithOprator("src/test/resouces/multi_parser") match {
        case Right(Prog(x)) => {
          assert(x(0) == Empty())
          assert(x(1) == Empty())
          assert(x(2) == Empty())
          assert(x(3) == Empty())
          assert(x(4) == Prim(Op("+"), Prim(Op("-"), IntLit(1), IntLit(2)), Prim(Op("+"), IntLit(1), IntLit(2))))
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

    def parseWithOprator(filename: String): Either[String, Prog] = {
      val parser = new Parser
      val buff = readfileAll(filename)
      parser.parseWithOperator(buff)
    }

    def readfileAll(filename: String): String = {
      val source = Source.fromFile(filename)
      val lines = source.getLines.toList
      lines.mkString("\n")
    }
  }

  def parseLine(in: String) = {
    val parser = new Parser
    parser.parse(in + ";") match {
      case Right(Prog(x)) => x(0)
      case Left(x) => throw new Exception(x)
    }
  }
}
