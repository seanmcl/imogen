package imogen

import org.scalatest.FunSuite

class ParserSuite extends FunSuite {
  def ok(s: String, f: Formula) = assert(Parse.parseString(s) === f)
  def err(s: String) = intercept[ParseError](Parse.parseString(s))

  test("Empty") {
    err("")
  }

  test("Top") {
    ok("T", Top)
  }

  test("Atoms") {
    def atom(s: String) = ok(s, Atom(s))
    def notAtom(s: String) = err(s)
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
    ok("p & q & r", And(Atom("p"), And(Atom("q"), Atom("r"))))
    ok("T & T & T & T", And(Top, And(Top, And(Top, Top))))
    ok("(T & T) & T", And(And(Top, Top), Top))
  }

  test("Implication Right Associative") {
    ok("p => q => r", Imp(Atom("p"), Imp(Atom("q"), Atom("r"))))
    ok("T => T => T => T", Imp(Top, Imp(Top, Imp(Top, Top))))
    ok("(T => T) => T", Imp(Imp(Top, Top), Top))
  }

  test("Implication Conjunction Precedence") {
    ok("p & q => r", Imp(And(Atom("p"), Atom("q")), Atom("r")))
    ok("p => q & r", Imp(Atom("p"), And(Atom("q"), Atom("r"))))
    ok("T => T & T => T", Imp(Top, Imp(And(Top, Top), Top)))
  }

}
