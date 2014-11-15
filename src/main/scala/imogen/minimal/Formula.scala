package imogen.minimal

import imogen.minimal.Atom.Label
import imogen.util.LabelFactory

sealed trait Formula

case class Atom(label: Label) extends Formula
case class And(left: Formula, right: Formula) extends Formula
case object Top extends Formula
case class Imp(left: Formula, right: Formula) extends Formula

object Atom {
  val labels = LabelFactory.create("p")
  type Label = labels.T
}
