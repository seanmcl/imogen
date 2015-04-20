package imogen

sealed trait Formula

final case class Atom(name: Atom.Symbol) extends Formula
final case class And(left: Formula, right: Formula) extends Formula
case object Top extends Formula
final case class Imp(left: Formula, right: Formula) extends Formula
final case class Iff(left: Formula, right: Formula) extends Formula

object Atom {
  val symbols = new imogen.Symbol("p")
  type Symbol = symbols.T
}
