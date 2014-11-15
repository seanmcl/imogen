package imogen.minimal.nd

sealed trait Intro

case class Pair(left: Intro, right: Intro) extends Intro
case class Lam(bvar: String, body: Intro) extends Intro
case object Unit extends Intro
case class Cast(elim: Elim) extends Intro
case class Let(bvar: String, bval: Elim, body: Intro)

sealed trait Elim

case class Label(label: String) extends Elim
case class Fst(body: Elim) extends Elim
case class Snd(body: Elim) extends Elim
case class App(fun: Elim, arg: Intro) extends Elim

object ND {
  def normalize(term: Intro): Intro = term match {
    case Pair(t1, t2) => Pair(normalize(t1), normalize(t2))
    case Unit => Unit
  }

  def normalize(term: Elim): Elim = term match {
    case Label(_) => term
  }
}


