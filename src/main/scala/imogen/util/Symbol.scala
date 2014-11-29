package imogen.util

final class Symbol(prefix: String) {
  private var ctr = 0

  sealed abstract class T

  case class Interned(s: String) extends T {
    override def toString = s
  }

  case class Nexted(s: Option[String], n: Int) extends T {
    override def toString = s match {
      case Some(s_) => s_ + "_" + n
      case None => prefix + "_" + n
    }
  }

  def intern(s: String): T = Interned(s)

  def next(prefix: Option[String] = None): T = {
    val t = Nexted(prefix, ctr)
    ctr = ctr + 1
    t
  }

  def equal(t1: T, t2: T) = (t1, t2) match {
    case (Interned(p1), Interned(p2)) => p1 == p2
    case (Nexted(_, n1), Nexted(_, n2)) => n1 == n2
    case _ => false
  }
}
