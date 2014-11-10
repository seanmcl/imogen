package imogen

class ParseError extends Exception

class Parser extends scala.util.parsing.combinator.RegexParsers {
  val atomRegex = """[_a-z][_a-zA-Z0-9]*""".r
  def atom: Parser[Formula] = "T" ^^ (_ => Top) | atomRegex ^^ Atom | "(" ~> form <~ ")"
  def conj: Parser[Formula] = atom ~ ("&" ~> conj).? ^^ {
    case x ~ None => x
    case x ~ Some(y) => And(x, y)
  }
  def form: Parser[Formula] = conj ~ ("=>" ~> form).? ^^ {
    case x ~ None => x
    case x ~ Some(y) => Imp(x, y)
  }
  def parseString(s: String): Formula = {
    parseAll(form, s) match {
      case Success(result, next) => result
      case _: NoSuccess => throw new ParseError
    }
  }
}
