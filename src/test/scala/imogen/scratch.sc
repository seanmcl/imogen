val x = 5

def next: scalaz.State[Int, Int] = for {
  x <- scalaz.Scalaz.get
  _ <- scalaz.Scalaz.put(x + 1)
} yield x

val y = for {
  x <- next
  y <- next
} yield x + y

val z = y.eval(10)




