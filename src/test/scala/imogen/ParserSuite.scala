package imogen

import imogen.ParserUtil.Interpolation
import org.scalatest.FunSuite

import scala.util.{Failure, Success}

class ParserSuite extends FunSuite {

  def success(s: String, t: Intro) =
    try {
      Parser.parseTerm(s) match {
        case Success(t1) => {
          println("Parsed   : " + t1)
          println("Expected : " + t)
          val msg =
            "Not alpha equivalent:\nParsed   : " + t1 + "\nExpected : " + t
          assert(Alpha(t1, t), msg)
        }
        case Failure(exn) => fail(exn)
      }
    } catch {
      case e: Exception => fail(e)
    }

  def failure(s: String, t: Intro) = Parser.parseTerm(s) match {
    case Success(t1) => assert(!Alpha(t1, t),
      t1 + "should not be alpha equivalent to " + t)
    case Failure(exn) => fail(exn)
  }

  def error(s: String) = Parser.parseTerm(s) match {
    case Success(_) => fail("Expected failure, got success.")
    case Failure(_) => ()
  }

  test("unit") {
    success("<>", Unit)
    success("(<>)", Unit)
    error("<> foo")
    error("<> <>")
    error("()")
  }

  test("vars") {
    success("x", i"x")
    failure("x", i"y")
    error("x^")
  }

  test("lam") {
    val x = ND.labels.intern("x")
    success("fn x => x", Lam(x, Cast(Label(x))))
    success("(fn x => x)", Lam(x, Cast(Label(x))))
  }

  test("ascribe") {
    success("<> : T", Cast(Ascribe(Unit, form"T")))
    success("(<> : T)", Cast(Ascribe(Unit, form"T")))
    success("((x : T))", Cast(Ascribe(i"x", form"T")))
    success("(((fn x => x) : (T)))", Cast(Ascribe(Lam(l"x", i"x"), form"T")))
    success("(<>:T) <>", Cast(App(Ascribe(Unit, Top), Unit)))
    success("(((fn x => (x: t1 => t2)) : (T)))",
      Cast(Ascribe(Lam(l"x", Cast(Ascribe(i"x", form"t1 => t2"))), form"T")))
  }

  test("funs") {
    success("fn x => x", Lam(l"x", i"x"))
    success("f x", Cast(App(e"f", i"x")))
    success("(f: T => T) x", Cast(App(Ascribe(i"f", form"T => T"), i"x")))
    success("(f: T => T => T) x y",
      Cast(App(App(Ascribe(i"f", form"T => T => T"), i"x"), i"y")))
  }

  test("let") {
    success("let x = y in x", Let(l"x", e"y", i"x"))
    success("let f = ((fn x => x) : T => T) in f <>",
      Let(l"f", Ascribe(Lam(l"y", i"y"), form"T => T"), Cast(App(e"f", Unit))))
  }

  test("bug") {
  }
}

