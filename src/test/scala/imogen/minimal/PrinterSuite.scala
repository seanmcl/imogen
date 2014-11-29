package imogen.minimal

import org.scalatest.FunSuite

import scala.util.{Failure, Success}

class PrinterSuite extends FunSuite {
  def eq(s: String, expected: String): Unit = {
    Parser.parseTerm(s) match {
      case Success(t) =>
        assert(Printer.pretty(t) == expected)
      case Failure(exn) => fail(exn)
    }
  }

  def neq(s: String, not_expected: String): Unit = {
    Parser.parseTerm(s) match {
      case Success(t) => assert(Printer.pretty(t) != not_expected)
      case Failure(exn) => fail(exn)
    }
  }

  test("vars") {
    eq("x", "x")
    neq("x", "y")
  }

  test("lam") {
    eq("fn x => x", "fn x => x")
  }
}
