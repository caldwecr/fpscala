package ch2

object Sorting {

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n + 1 >= as.length) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false
    }
    loop(0)
  }

  def main(args: Array[String]): Unit = {
    val foo = Array(1, 2, 3, 4, 5)
    val bar = Array(2, 1, 3, 4, 5)

    val fooSorted = isSorted[Int](foo, (a, b) => a <= b)
    val barSorted = isSorted[Int](bar, (a, b) => a <= b)
    println(s"foo is sorted = $fooSorted")
    println(s"bar is sorted = $barSorted")
  }
}

