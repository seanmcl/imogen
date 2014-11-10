package imogen

import org.scalatest.FunSuite

class ParserSuite extends FunSuite {
  def ok(s: String, f: Formula) = assert(Parse.parseString(s) === f)
  def err(s: String) = intercept[ParseError](Parse.parseString(s))

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
}
