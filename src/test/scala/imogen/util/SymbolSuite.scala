package imogen.util

import org.scalatest.FunSuite

class SymbolSuite extends FunSuite {
  test("equality") {
    val labels = new Symbol("x")
    val l0 = labels.next()
    val l1 = labels.next()
    val l2 = labels.next(Some("foo"))
    val l3 = labels.intern("x_0")
    assert(labels.equal(l0, l0))
    assert(!labels.equal(l0, l1))
    assert(l0.toString == "x_0")
    assert(l1.toString == "x_1")
    assert(l2.toString == "foo_2")
    assert(!labels.equal(l0, l3))
  }
}
