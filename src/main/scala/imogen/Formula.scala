package imogen

sealed abstract class Formula() {}
case class Atom(name: String) extends Formula
case class And(left: Formula, right: Formula) extends Formula
case object Top extends Formula
case class Imp(left: Formula, right: Formula) extends Formula
