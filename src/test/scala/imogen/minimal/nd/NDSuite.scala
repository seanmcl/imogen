package imogen.minimal.nd

import imogen.minimal.Parser
import org.scalatest.FunSuite
import scala.util.{Failure, Success}

object LabelInterpolation {
  implicit class LabelMakerQ(val sc: StringContext) extends AnyVal {
    private def fail = throw new RuntimeException("ND interpolation doesn't support arguments.")
    def l(args: Any*): ND.Symbol =
      if (args.length > 0) fail else ND.labels.intern(sc.parts.mkString(""))
    def e(args: Any*): Elim =
      if (args.length > 0) fail else Label(ND.labels.intern(sc.parts.mkString("")))
    def i(args: Any*): Intro =
      if (args.length > 0) fail else Cast(Label(ND.labels.intern(sc.parts.mkString(""))))
  }
}

class AlphaSuite extends FunSuite {
  def success(s: String, t: String) = (Parser.parseTerm(s), Parser.parseTerm(t)) match {
    case (Success(s1), Success(t1)) => assert(Alpha(s1, t1))
    case _ => fail("Parse")
  }
}

class NormalizeSuite extends FunSuite {
  def success(t1: Intro, t2: Intro) = assert(Normalize(t1) == Success(t2))
  def success(t1: Elim, t2: Intro) = assert(Normalize(t1) == Success(t2))
  def failure(t1: Intro) = assert(Normalize(t1) == Failure(Check.Check()))

  test("Unit") {
    success(Unit, Unit)
  }
}

class CheckSuite extends FunSuite {
  def success(t_str: String, f_str: String) = (Parser.parseTerm(t_str), Parser.parseFormula(f_str)) match {
    case (Success(t), Success(f)) => Check(t, f) match {
      case Success(()) => ()
      case Failure(exn) => fail(exn)
    }
    case _ => fail("Parse error")
  }

  def failure(t_str: String, f_str: String) = (Parser.parseTerm(t_str), Parser.parseFormula(f_str)) match {
    case (Success(t), Success(f)) => Check(t, f) match {
      case Success(()) => fail("Expected failure, got success")
      case Failure(exn) => ()
    }
    case _ => fail("Parse error")
  }

//  test("1") {
//    success("fn xy => <snd xy, fst xy>", "p & q => q & p")
//  }
}
