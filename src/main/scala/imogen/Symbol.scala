package imogen

trait Symbol {
  val prefix: String

  sealed trait T

  case class String(s: String) extends T {
    override def toString = s
  }

  case class Int(s: Option[String], n: Int) extends T {
    override def toString = s match {
      case Some(s_) => s_ + "_" + n
      case None => prefix + "_" + n
    }
  }

  def equal(t1: T, t2: T) = (t1, t2) match {
    case (String(p1), String(p2)) => p1 == p2
    case (Int(_, n1), Int(_, n2)) => n1 == n2
    case _ => false
  }
}

object Symbol {
  type State[A] = scalaz.State[SymbolState, A]
  def next(prefix: Option[String]): State[Symbol] = for {
    n <- get.counter
    _ <- put(SymbolState(n+1))
  } yield Int(prefix, n)
}

trait SymbolState {
  val counter: Int
}

object SymbolState {
  def apply(n: Int): SymbolState = new SymbolState{ val counter = n }
}

trait Var extends Symbol {
  val prefix = "x"
}

trait Param extends Symbol {
  val prefix = "a"
}
