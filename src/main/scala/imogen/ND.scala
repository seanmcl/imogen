package imogen

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


