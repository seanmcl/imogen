package imogen.minimal.formula

import imogen.util.ParseError
import org.scalatest.FunSuite

import scala.util.{Failure, Success}

class FormulaSuite extends FunSuite {
  def ok(s: String, f: Formula) = Parser.parseString(s) match {
    case Success(f1) => assert(f === f1)
    case Failure(exn) => assert(false, exn.toString)
  }
  def err(s: String) = Parser.parseString(s) match {
    case Failure(_) => ()
    case Success(_) => assert(false, "Expected parse error")
  }
  def mkAtom(s: String) = Atom(Atom.labels.intern(s))
  def atom(s: String) = ok(s, mkAtom(s))
  def notAtom(s: String) = err(s)

  test("Empty") {
    err("")
  }

  test("Top") {
    ok("T", Top)
  }

  test("Atoms") {
    atom("x")
    atom("_x")
    atom("x1")
    atom("x_1")
    atom("x1A")
    atom("x1A")
    atom("x1A")
    notAtom("A")
    notAtom("1")
    notAtom("1a")
  }

  test("Conjunction Right Associative") {
    ok("p & q & r", And(mkAtom("p"), And(mkAtom("q"), mkAtom("r"))))
    ok("T & T & T & T", And(Top, And(Top, And(Top, Top))))
    ok("(T & T) & T", And(And(Top, Top), Top))
  }

  test("Implication Right Associative") {
    ok("p => q => r", Imp(mkAtom("p"), Imp(mkAtom("q"), mkAtom("r"))))
    ok("T => T => T => T", Imp(Top, Imp(Top, Imp(Top, Top))))
    ok("(T => T) => T", Imp(Imp(Top, Top), Top))
  }

  test("Implication Conjunction Precedence") {
    ok("p & q => r", Imp(And(mkAtom("p"), mkAtom("q")), mkAtom("r")))
    ok("p => q & r", Imp(mkAtom("p"), And(mkAtom("q"), mkAtom("r"))))
    ok("T => T & T => T", Imp(Top, Imp(And(Top, Top), Top)))
  }
}
