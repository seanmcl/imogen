package imogen.util

import org.scalatest.FunSuite

class LabelFactorySuite extends FunSuite {
  test("labels") {
    val f1 = LabelFactory.create("x")
    val l0 = f1.next()
    val l1 = f1.next()
    val l2 = f1.next()
    assert (f1.eq(l0, l0))
    assert (f1.eq(l0, l0))
    assert (!f1.eq(l0, l1))
    assert (f1.show(l0) == "x")
    assert (f1.show(l1) == "x1")
    assert (f1.show(l2) == "x2")
  }
}
