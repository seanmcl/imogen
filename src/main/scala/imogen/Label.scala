package imogen

class Label(base: String = "x") {
  private var next = 0
  private Map[Int, String] intToName = Map.empty
  private Map[String, Int] nameToInt = Map.empty
  def next() = {
    val ind = next
    next
  }

}
