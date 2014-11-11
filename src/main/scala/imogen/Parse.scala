package imogen

import scala.io.Source

object Parse extends Parser {
  def main(args: Array[String]) {
    val src = Source.fromFile(args(0))
    src.getLines().foreach {
      line => println(parseString(line))
    }
    // TODO: Use RAII instead of explicit close.
    src.close()
  }
}
