package imogen

sealed trait Pos

final case class PAtom(name: Atom.Symbol) extends Pos
final case class Tensor(left: Pos, right: Pos) extends Pos
final case class Down(left: Neg) extends Pos

sealed trait Neg
final case class NAtom(name: Atom.Symbol) extends Neg
final case class With(left: Neg, right: Neg) extends Neg
final case class Lolli(left: Pos, right: Neg) extends Neg
final case class Up(left: Pos) extends Neg


case object One extends Pos


object PAtom {
  val symbols = new imogen.Symbol("p")
  type Symbol = symbols.T
}

object NAtom {
  val symbols = new imogen.Symbol("q")
  type Symbol = symbols.T
}
