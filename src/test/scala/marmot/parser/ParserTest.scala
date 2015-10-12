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

  describe("Macro Parser") {
    val parser = new Parser
    val lines  = readfile("src/test/resouces/macrofile")

    val x = parser.parse(lines(0), 1) match {
      case Right(Prog(x)) => x(0)
      case Left(x) => throw new Exception(x)
    }
    val y = parser.parse(lines(1), 0) match {
      case Right(Prog(x)) => x(0)
      case Left(x) => throw new Exception(x)
    }

    println(y)
  }

  describe ("tri") {
    println("---tri---")
    val parser = new Parser
    val lines  = readfile("src/test/resouces/tri")

    val x = parser.parse(lines(0), 1) match {
      case Right(Prog(x)) => x(0)
      case Left(x) => throw new Exception(x)
    }
    val y = parser.parse(lines(1), 0) match {
      case Right(Prog(x)) => x(0)
      case Left(x) => throw new Exception(x)
    }
    println("==========")
    println(y)
    println("==========")
  }

  describe ("lambda") {
    println("---lambda---")
    val parser = new Parser
    val lines  = readfile("src/test/resouces/lambda")

    val x = parser.parse(lines(0), 1) match {
      case Right(Prog(x)) => x(0)
      case Left(x) => throw new Exception(x)
    }
    println(x)
    val y = parser.parse(lines(1), 0) match {
      case Right(Prog(x)) => x(0)
      case Left(x) => throw new Exception(x)
    }
    println("==========")
    println(y)
    println("==========")
  }

  def readfile(filename: String) = {
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    lines
    // lines.tail.foldLeft(lines.head) { (a, e) => a + "\n" + e }
  }

  def parseLine(in: String, i: Int = 0) = {
    val parser = new Parser
    parser.parse(in, i) match {
      case Right(Prog(x)) => x(0)
      case Left(x) => throw new Exception(x)
    }
  }
}
