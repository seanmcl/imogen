package imogen.minimal.nd

import imogen.minimal.formula.{ Top, Formula, Imp, And }
import scala.util.{ Failure, Success, Try }

object ND {
  val labels = new imogen.util.Symbol("H")
  type Symbol = labels.T
}

sealed trait Intro
final case class Pair(left: Intro, right: Intro) extends Intro
final case class Lam(bvar: ND.Symbol, body: Intro) extends Intro
case object Unit extends Intro
final case class Cast(elim: Elim) extends Intro
final case class Let(bvar: ND.Symbol, bval: Elim, body: Intro) extends Intro

sealed trait Elim
final case class Label(label: ND.Symbol) extends Elim
final case class Fst(body: Elim) extends Elim
final case class Snd(body: Elim) extends Elim
final case class App(fun: Elim, arg: Intro) extends Elim
final case class Ascribe(term: Intro, typ: Formula) extends Elim

object Alpha {
  type Ctx = List[(ND.Symbol, ND.Symbol)]
  def eqi(ctx: Ctx): (Intro, Intro) => Boolean = {
    case (Unit, Unit) => true
    case (Pair(r1, r2), Pair(s1, s2)) => eqi(ctx)(r1, s1) & eqi(ctx)(r2, s2)
    case (Cast(r), Cast(s)) => eqe(ctx)(r, s)
    case (Lam(x1, r1), Lam(x2, r2)) => eqi((x1, x2) :: ctx)(r1, r2)
    case (Let(x1, v1, b1), Let(x2, v2, b2)) => eqe(ctx)(v1, v2) & eqi((x1, x2) :: ctx)(b1, b2)
    case _ => false
  }
  def eqe(ctx: Ctx): (Elim, Elim) => Boolean = {
    case (Label(x1), Label(x2)) => ctx.find { case (x, _) => ND.labels.equal(x, x1) } match {
      case None => ND.labels.equal(x1, x2)
      case Some((_, y)) => ND.labels.equal(x2, y)
    }
    case (Fst(t1), Fst(t2)) => eqe(ctx)(t1, t2)
    case (Snd(t1), Snd(t2)) => eqe(ctx)(t1, t2)
    case (App(f1, a1), App(f2, a2)) => eqe(ctx)(f1, f2) & eqi(ctx)(a1, a2)
    case (Ascribe(t1, f1), Ascribe(t2, f2)) => f1 == f2 & eqi(ctx)(t1, t2)
    case _ => false
  }
  def apply(t1: Intro, t2: Intro) = eqi(Nil)(t1, t2)
  def apply(t1: Elim, t2: Elim) = eqe(Nil)(t1, t2)
}

object Normalize {
  final case class Normalize(msg: String = "") extends RuntimeException(msg)

  private class Ctx private (map: Map[ND.Symbol, Intro]) {
    def extend(key: ND.Symbol, value: Intro): Ctx = {
      new Ctx(map + (key -> value))
    }
    def apply(x: ND.Symbol) = map.get(x) match {
      case Some(t) => t
      case None => throw new Normalize("Unbound variable: " + x)
    }
  }

  private object Ctx {
    type T = Map[ND.Symbol, Intro]
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
    case Ascribe(t, _) => normi(ctx)(t)
  }

  def apply(x: Intro) = normi(Ctx.id)(x)
  def apply(x: Elim) = norme(Ctx.id)(x)
}

object Check {
  final case class Check(msg: String = "") extends RuntimeException(msg)
  private type Ctx = List[(ND.Symbol, Formula)]
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
    case Label(x) => ctx.find { case (y, _) => ND.labels.equal(x, y) } match {
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
      case Imp(p1, p2) => check(ctx)(t2, p1).map { _ => p2 }
      case _ => fail
    }
    case Ascribe(t, f) => check(ctx)(t, f) map { _ => f }
  }

  def apply(t: Intro, f: Formula) = check(Nil)(t, f)
}

