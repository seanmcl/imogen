package imogen.util

import org.scalatest.FunSuite

class LabelSuite extends FunSuite {
  test("labels") {
    val f1 = LabelFactory.create("x")
    val l0 = f1.next()
    val l1 = f1.next()
    val l2 = f1.next()
    val l3 = f1.intern("t")
    val l4 = f1.intern("t")
    val l5 = f1.intern("x_0")
    assert (f1.eq(l0, l0))
    assert (!f1.eq(l0, l1))
    assert (f1.show(l0) == "x_0")
    assert (f1.show(l1) == "x_1")
    assert (f1.show(l2) == "x_2")
    assert (f1.eq(l3, l4))
    assert (!f1.eq(l0, l5))
  }
}
