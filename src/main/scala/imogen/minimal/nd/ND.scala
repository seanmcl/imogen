package imogen.minimal.nd

import imogen.minimal.formula.{Top, Formula, Imp, And}
import imogen.util.{ParseError, LabelFactory}
import scala.util.{Failure, Success, Try}

object ND {
  val labels = LabelFactory.create("H")
  type Label = labels.T
}

sealed trait Intro
final case class Pair(left: Intro, right: Intro) extends Intro
final case class Lam(bvar: ND.Label, body: Intro) extends Intro
case object Unit extends Intro
final case class Cast(elim: Elim) extends Intro
final case class Let(bvar: ND.Label, bval: Elim, body: Intro) extends Intro

sealed trait Elim
final case class Label(label: ND.Label) extends Elim
final case class Fst(body: Elim) extends Elim
final case class Snd(body: Elim) extends Elim
final case class App(fun: Elim, arg: Intro) extends Elim
final case class Ascribe(term: Intro, typ: Formula) extends Elim

object Alpha {
  type Ctx = List[(ND.Label, ND.Label)]
  def eqi(ctx: Ctx, t1: Intro, t2: Intro): Boolean = (t1, t2) match {
    case (Unit, Unit) => true
    case (Pair(r1, r2), Pair(s1, s2)) => eqi(ctx, r1, s1) & eqi(ctx, r2, s2)
    case (Cast(r), Cast(s)) => eqe(ctx, r, s)
    case (Lam(x1, r1), Lam(x2, r2)) => eqi((x1, x2) :: ctx, r1, r2)
    case (Let(x1, v1, b1), Let(x2, v2, b2)) => eqe(ctx, v1, v2) & eqi((x1, x2) :: ctx, b1, b2)
    case _ => false
  }
  def eqe(ctx: Ctx, t1: Elim, t2: Elim): Boolean = (t1, t2) match {
    case (Label(x1), Label(x2)) => ctx.find { case (x, _) => ND.labels.eq(x, x1) } match {
      case None => ND.labels.eq(x1, x2)
      case Some((_, y)) => ND.labels.eq(x2, y)
    }
    case (Fst(t1), Fst(t2)) => eqe(ctx, t1, t2)
    case (Snd(t1), Snd(t2)) => eqe(ctx, t1, t2)
    case (App(f1, a1), App(f2, a2)) => eqe(ctx, f1, f2) & eqi(ctx, a1, a2)
    case (Ascribe(t1, f1), Ascribe(t2, f2)) => f1 == f2 & eqi(ctx, t1, t2)
    case _ => false
  }
  def apply(t1: Intro, t2: Intro) = eqi(Nil, t1, t2)
  def apply(t1: Elim, t2: Elim) = eqe(Nil, t1, t2)
}

object Normalize {
  final case class Normalize(msg: String = "") extends RuntimeException(msg)
  
  private class Ctx private (map: Map[ND.Label, Intro]) {
    def extend(key: ND.Label, value: Intro): Ctx = {
      new Ctx(map + (key -> value))
    }
    def apply(x: ND.Label) = map.get(x) match {
      case Some(t) => t
      case None => throw new Normalize("Unbound variable: " + ND.labels.show(x))
    }
  }

  private object Ctx {
    type T = Map[ND.Label, Intro]
    val id = new Ctx(Map.empty)
  }

  private def normi(ctx: Ctx): Intro => Try[Intro] = {
    case Pair(a, b) => for {
      na <- normi(ctx)(a)
      nb <- normi(ctx)(b)
    } yield Pair(na, nb)
    case Unit => Success(Unit)
    case Cast(e) => norme(ctx)(e)
    case Let(x, v, b) => for {
      vn <- norme(ctx)(v)
      bn <- normi(ctx.extend(x, vn))(b)
    } yield bn
    case Lam(x, b) =>
      val x1 = ND.labels.next()
      for {
        bn <- normi(ctx.extend(x1, Cast(Label(x))))(b)
      } yield Lam(x1, bn)
  }

  private def norme(ctx: Ctx): Elim => Try[Intro] = {
    case Label(x) => Try(ctx(x))
    case Fst(t) => norme(ctx)(t) flatMap {
      case Pair(t1, t2) => Success(t1)
      case Cast(e) => Success(Cast(Fst(e)))
      case _ => Failure(new Normalize)
    }
    case Snd(t) => norme(ctx)(t) flatMap {
      case Pair(t1, t2) => Success(t2)
      case Cast(e) => Success(Cast(Snd(e)))
      case _ => Failure(new Normalize)
    }
    case App(t1, t2) =>
      normi(ctx)(t2) flatMap (n2 => {
        norme(ctx)(t1) flatMap {
          case Lam(x, b) => normi(ctx.extend(x, n2))(b)
          case Cast(e1) => Success(Cast(App(e1, n2)))
          case _ => Failure(new Normalize)
        }
      })
  }

  def apply(x: Intro)= normi(Ctx.id)(x)
  def apply(x: Elim)= norme(Ctx.id)(x)
}

object Check {
  final case class Check(msg: String = "") extends RuntimeException(msg)
  private type Ctx = List[(ND.Label, Formula)]
  def fail[T]: Try[T] = Failure(new Check)
  def succ = Success(())
  def succ(x: Formula) = Success(x)

  private def check(ctx: Ctx): (Intro, Formula) => Try[Unit] = {
    case (Unit, Top) => succ
    case (Pair(t1, t2), And(p1, p2)) => for {
      () <- check(ctx)(t1, p1)
      () <- check(ctx)(t2, p2)
    } yield succ
    case (Lam(x, a), Imp(p1, p2)) => check((x -> p1) :: ctx)(a, p2)
    case (Cast(e), p) => syn(ctx)(e).map(p1 => if (p == p1) succ else fail)
    case (Let(x, v, b), p) => syn(ctx)(v).flatMap(q => check((x -> q) :: ctx)(b, p))
    case _ => fail
  }

  private def syn(ctx: Ctx): Elim => Try[Formula] = {
    case Label(x) => ctx.find {case (y, _) => ND.labels.eq(x, y)} match {
      case Some((_, p)) => succ(p)
      case None => fail
    }
    case Fst(t) => syn(ctx)(t).flatMap {
      case And(p1, p2) => succ(p1)
      case _ => fail
    }
    case Snd(t) => syn(ctx)(t).flatMap {
      case And(p1, p2) => succ(p2)
      case _ => fail
    }
    case App(t1, t2) => syn(ctx)(t1).flatMap {
      case Imp(p1, p2) => check(ctx)(t2, p1).map {_ => p2}
      case _ => fail
    }
  }

  def apply(t: Intro, f: Formula) = check(Nil)(t, f)
}

/**
 * T ::= ()
 *     | x
 *     | fn x => b
 *     | t1(t2)
 *     | fst(t)
 *     | snd(t)
 *     | let x1 = t1 in t
 *     | (t1, t2)
 *     | (t)
 */
object Parser extends scala.util.parsing.combinator.RegexParsers {
  import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}

  private val varRegex = """[a-z][_a-zA-Z0-9]*""".r

  def intro: Parser[Intro] = (
    "<>" ^^ (_ => Unit)
      | "(" ~> intro <~ ")"
      | ("<" ~> intro <~ ",") ~ (intro <~ ">") ^^ { case t1 ~ t2 => Pair(t1, t2) }
      | "fn" ~> varRegex ~> "=>" ~ intro ^^ { case x ~ t => Lam(ND.labels.intern(x), t) }
      | ("let" ~> varRegex <~ "=") ~ elim ~ ("in" ~> intro) ^^ { case x ~ t1 ~ t2 => Let(ND.labels.intern(x), t1, t2) }
      | elim ^^ Cast
    )

  def elim1: Parser[Elim] = (
    "(" ~> elim <~ ")"
      | "fst" ~> elim ^^ Fst
      | "snd" ~> elim ^^ Snd
      | varRegex ^^ (x => Label(ND.labels.intern(x)))
    )

  def elim: Parser[Elim] = (
    elim1 ~ intro.? ^^ {
      case t1 ~ None => t1
      case t1 ~ Some(t2) => App(t1, t2)
    }
  )

  def parseString(s: String): Try[Intro] = parseAll(intro, s) match {
    case Success(p, next) =>
      if (next.atEnd) TrySuccess(p) else
        TryFailure(ParseError("ND interpolation doesn't support arguments."))
    case _: NoSuccess => TryFailure(ParseError())
  }

  implicit class Interpolation(val sc: StringContext) extends AnyVal {
    def lam(args: Any*): Intro =
      if (args.length > 0) {
        throw ParseError("ND interpolation doesn't support arguments.")
      } else parseString(sc.parts.mkString("")) match {
        case TrySuccess(result: Intro) => result
        case TryFailure(exn) => throw exn
      }
  }
}