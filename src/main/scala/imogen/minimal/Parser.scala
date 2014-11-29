package imogen.minimal

import imogen.minimal.formula.{ Imp, And, Formula, Atom, Top }
import imogen.minimal.nd.{ Snd, Fst, Label, Cast, Ascribe, Let, App, ND, Lam, Elim, Intro, Unit, Pair }
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.{ Failure, Success }
import scala.util.parsing.input.Position
import scala.util.parsing.combinator.PackratParsers


/**
 * It's too difficult to parse the bi-directional form directly.  Clauses such as
 * Elem Intro for application, Elim : Form for ascription, and Cast(Elim) for an upcast
 * make the parsing rules ambiguous.  Parse into an intermediate form, and then walk over the
 * parse tree and find a minimal bi-directional encoding of the term.
 *
 * T ::= ()
 *     | x
 *     | fn x => b
 *     | t1 t2
 *     | let x1 = t1 in t
 *     | t : f
 *     | (t1, t2)
 *     | (t)
 */

// https://github.com/Villane/lambdacalculus/blob/master/src/lambda/LambdaParser.scala
case class ParseError(msg: String = "",
                      pos: Option[Position] = None) extends Exception {
  override def toString = pos match {
    case None => msg
    case Some(p) => msg + " at position: " + p
  }
}

object Parser extends StandardTokenParsers with PackratParsers {
  import scala.util.{ Try, Success => TrySuccess, Failure => TryFailure }

  lexical.reserved ++= Seq("let", "in", "fn", "T")
  lexical.delimiters ++= Seq("(", ")", "=>", "=", "&", "<", ">", ",", ":")

  def isLowerId(s: String) = {
    val c = s.charAt(0)
    c.isLower || c.equals('_')
  }

  private def atom: Parser[Formula] = (
    keyword("T") ^^ (_ => Top)
    | ident ^^ {case a if isLowerId(a) => Atom(Atom.symbols.intern(a))}
    | "(" ~> form <~ ")")

  private def conj: Parser[Formula] = atom ~ ("&" ~> conj).? ^^ {
    case x ~ None => x
    case x ~ Some(y) => And(x, y)
  }

  private def form: Parser[Formula] = conj ~ ("=>" ~> form).? ^^ {
    case x ~ None => x
    case x ~ Some(y) => Imp(x, y)
  }

  private sealed trait Term

  private case object TUnit extends Term
  private case class TVar(name: String) extends Term
  private case class TLam(bvar: String, body: Term) extends Term
  private case class TApp(fun: Term, arg: Term) extends Term
  private case class TFst(body: Term) extends Term
  private case class TSnd(body: Term) extends Term
  private case class TLet(bvar: String, value: Term, body: Term) extends Term
  private case class TAscribe(term: Term, typ: Formula) extends Term
  private case class TPair(left: Term, right: Term) extends Term

  private def pair: Parser[Term] = (
    "<" ~ ">" ^^ (_ => TUnit)
    | ("<" ~> term <~ ",") ~ (term <~ ">") ^^ {
      case t1 ~ t2 => TPair(t1, t2)
    })

  private def atomic: Parser[Term] = (
    ident ^? {case x if isLowerId(x) => TVar(x)}
    | pair
    | "(" ~> term <~ ")")

  private def term1: Parser[Term] = (
    (atomic <~ ":") ~ form ^^ { case t ~ f => TAscribe(t, f) }
    | (keyword("let") ~> ident <~ "=") ~ term ~ (keyword("in") ~> term) ^^ {
      case x ~ t1 ~ t2 => TLet(x, t1, t2)
    }
    | (keyword("fn") ~> ident) ~ ("=>" ~> term) ^^ { case x ~ t => TLam(x, t) }
    | atomic ~ atomic.* ^^ {
      case t ~ ts => ts match {
        case Nil => t
        case _ => ts.foldLeft(t)(TApp)
      }
    })

  //private def term = log(term1)("term")
  private def term = term1

  private def termToIntro(term: Term): Intro = term match {
    case TUnit => Unit
    case TLam(bvar, body) => Lam(ND.labels.intern(bvar), termToIntro(body))
    case TLet(bvar, value, body) =>
      Let(ND.labels.intern(bvar), termToElim(value), termToIntro(body))
    case TPair(left, right) => Pair(termToIntro(left), termToIntro(right))
    case _ => Cast(termToElim(term))
  }

  private def termToElim: Term => Elim = {
    case TVar(x) => Label(ND.labels.intern(x))
    case TApp(TVar("fst"), arg) => Fst(termToElim(arg))
    case TApp(TVar("snd"), arg) => Snd(termToElim(arg))
    case TApp(fun, arg) => App(termToElim(fun), termToIntro(arg))
    case TAscribe(term, form) => Ascribe(termToIntro(term), form)
    case TFst(term) => Fst(termToElim(term))
    case TSnd(term) => Snd(termToElim(term))
    case _ => throw ParseError("termToElim")
  }

  def parseReader[A](p: Parser[A])(in: String): Try[A] = {
    try {
      phrase(p)(new lexical.Scanner(in)) match {
        case Success(result, _) => TrySuccess(result)
        case fail: NoSuccess => TryFailure(ParseError(fail.msg))
      }
    } catch {
      case e: Exception => TryFailure(e)
    }
  }

  val parseFormula = parseReader(form)_
  val parseTerm = parseReader(term ^^ termToIntro)_
}

object Implicits {
  implicit class Interpolation(val sc: StringContext) extends AnyVal {
    import Parser.{ parseFormula, parseTerm }

    def form(args: Any*): Formula = {
      parseFormula(sc.parts.mkString("")) match {
        case Success(result) => result
        case Failure(exn) => throw exn
      }
    }

    def lam(args: Any*): Intro = {
      parseTerm(sc.parts.mkString("")) match {
        case Success(result: Intro) => result
        case Failure(exn) => throw exn
      }
    }

    def l(args: Any*): ND.Symbol = {
      ND.labels.intern(sc.parts.mkString(""))
    }

    def i(args: Any*): Intro = {
      Cast(Label(ND.labels.intern(sc.parts.mkString(""))))
    }

    def e(args: Any*): Elim = {
      Label(ND.labels.intern(sc.parts.mkString("")))
    }
  }
}