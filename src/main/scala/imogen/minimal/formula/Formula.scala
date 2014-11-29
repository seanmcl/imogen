package imogen.minimal.formula

sealed trait Formula

final case class Atom(name: Atom.Symbol) extends Formula

final case class And(left: Formula, right: Formula) extends Formula

case object Top extends Formula

final case class Imp(left: Formula, right: Formula) extends Formula

object Atom {
  val symbols = new imogen.util.Symbol("p")
  type Symbol = symbols.T
}
