package ch2

import org.scalatest._

class SortingSpec extends FlatSpec with Matchers {
  "isSorted" should "be true for sorted Arrays" in {
    val as = Array(1, 2, 3, 4, 5)
    Sorting.isSorted[Int](as, (x,y) => x <= y) shouldBe true
  }
  it should "be false for Arrays that are not sorted" in {
    val as = Array(2, 1, 3, 4, 5)
    Sorting.isSorted[Int](as, (x,y) => x <= y) shouldBe false
  }
}
