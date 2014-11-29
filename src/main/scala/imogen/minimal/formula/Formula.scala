package imogen.minimal.formula

import imogen.util.{LabelFactory, ParseError}

sealed trait Formula

final case class Atom(label: Atom.Label) extends Formula
final case class And(left: Formula, right: Formula) extends Formula
case object Top extends Formula
final case class Imp(left: Formula, right: Formula) extends Formula

object Atom {
  val labels = LabelFactory.create("p")
  type Label = labels.T
}

object Parser extends scala.util.parsing.combinator.RegexParsers {

  import scala.util.{Try, Failure => TryFailure, Success => TrySuccess}

  private val atomRegex = """[_a-z][_a-zA-Z0-9]*""".r

  private def atom: Parser[Formula] = (
    "T" ^^ (_ => Top)
      | atomRegex ^^ (a => Atom(Atom.labels.intern(a)))
      | "(" ~> form <~ ")"
    )

  private def conj: Parser[Formula] = atom ~ ("&" ~> conj).? ^^ {
    case x ~ None => x
    case x ~ Some(y) => And(x, y)
  }

  private def form: Parser[Formula] = conj ~ ("=>" ~> form).? ^^ {
    case x ~ None => x
    case x ~ Some(y) => Imp(x, y)
  }

  def parseString(s: String): Try[Formula] = {
    parseAll(form, s) match {
      case Success(result, next) =>
        if (next.atEnd) TrySuccess(result)
        else TryFailure(ParseError("Unparsed input: " + next.toString))
      case _: NoSuccess => TryFailure(ParseError())
    }
  }

  implicit class Interpolation(val sc: StringContext) extends AnyVal {
    def form(args: Any*): Formula =
      if (args.length > 0) {
        throw ParseError("ND interpolation doesn't support arguments.")
      } else parseString(sc.parts.mkString("")) match {
        case TrySuccess(result) => result
        case TryFailure(exn) => throw exn
      }
  }
}
