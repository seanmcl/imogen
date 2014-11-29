package imogen.util

import scala.collection.mutable

trait Label {
  type T
  def eq(x: T, y: T): Boolean
  def show(x: T): String
  def next(): T
  def intern(s: String): T
}

object LabelFactory {
  def create(prefix: String) = new Label {
    type T = Int
    private var ctr: Int = 0
    private val intToName: mutable.Map[Int, String] = mutable.HashMap.empty
    private val nameToInt: mutable.Map[String, Int] = mutable.HashMap.empty

    def show(n: T) = intToName.get(n) match {
      case Some(name) => name
      case None => prefix + "_" + n
    }

    def eq(x: T, y: T): Boolean = y.equals(x)

    def next() = {
      val index = ctr
      ctr = ctr + 1
      index
    }

    def intern(s: String) = {
      nameToInt.get(s) match {
        case Some(n) => n
        case None =>
          val n = next()
          intToName += (n -> s)
          nameToInt += (s -> n)
          n
      }
    }
  }
}
