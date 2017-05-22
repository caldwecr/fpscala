package fpinscala.datastructures

import org.scalatest._

class ListSpec extends FlatSpec with Matchers {

  "tail of List(1, 2, 3, 4, 5)" should "be Some List(2, 3, 4, 5)" in {
    List.tail(List(1, 2, 3, 4, 5)) shouldEqual Some(List(2, 3, 4, 5))
  }

  "tail of List(\"a\", \"b\", \"c\")" should "be Some List(\"b\", \"c\")" in {
    List.tail(List("a", "b", "c")) shouldEqual Some(List("b", "c"))
  }
}
