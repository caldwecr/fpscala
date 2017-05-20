package ch2

object Curry {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a => (b => f(a, b)))
  }
}
