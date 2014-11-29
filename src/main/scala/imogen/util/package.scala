package imogen

package object util {
  case class ParseError(msg: String = "") extends Exception
}
