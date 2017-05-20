object Fib {
  def main(args: Array[String]): Unit = {
    println(fib(0))
    println(fib(1))
    println(fib(2))
    println(fib(3))
    println(fib(4))
    println(fib(42))
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(i: Int, j: Int,  m: Int, l: Int): Int = {
      if (i == j - 1) l
      else if (i == j) m
      else go(i, j + 1, m + l, m)
    }

    go(n, 1, 1, 0)
  }
}
