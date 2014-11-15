package imogen.util

import scala.collection.mutable

trait Label {
  type T
  def eq(x: T, y: T): Boolean
  def show(x: T): String
  def next(prefix: Option[String] = None): T
}

object LabelFactory {
  def create(prefix: String): Label = {
    new Label {
      type T = Int
      private var ctr: Int = 0
      private val intToName: mutable.HashMap[Int, String] = mutable.HashMap.empty[Int, String]
      private val nameToInt = mutable.HashMap.empty[String, Int]
      // There's no None check necessary here, since any label was created by a call to [next]
      def show(n: T) = intToName.get(n).get
      def eq(x: T, y: T): Boolean = y.equals(x)
      def next(prefix_ : Option[String]) = {
        val index = ctr
        ctr = ctr + 1
        val postfix = if (index == 0) "" else index
        val name: String = prefix_.getOrElse(prefix) + postfix
        intToName += (index -> name)
        nameToInt += (name -> index)
        index
      }
    }
  }
}
