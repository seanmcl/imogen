import scala.util.parsing.combinator.syntactical.StandardTokenParsers

sealed abstract class Formula()
case class And(left: Formula, right: Formula) extends Formula
case object Top extends Formula
case class Imp(left: Formula, right: Formula) extends Formula






