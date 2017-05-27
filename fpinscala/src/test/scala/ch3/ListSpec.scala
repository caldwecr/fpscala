package fpinscala.datastructures

import org.scalatest._

class ListSpec extends FlatSpec with Matchers {

  "tail of List(1, 2, 3, 4, 5)" should "equal List(2, 3, 4, 5)" in {
    List.tail(List(1, 2, 3, 4, 5)) shouldEqual List(2, 3, 4, 5)
  }

  "tail of List(\"a\", \"b\", \"c\")" should "equal List(\"b\", \"c\")" in {
    List.tail(List("a", "b", "c")) shouldEqual List("b", "c")
  }

  "tail of Nil" should "be Nil" in {
    List.tail(Nil) shouldEqual Nil
  }

  "tail of List(1)" should "be Nil" in {
    List.tail(List(1)) shouldEqual Nil
  }

  "setHead" should "be the list with head at the front" in {
    List.setHead(List(3, 4), 7) shouldEqual List(7, 3, 4)
  }

  it should "create a List with one item if the tail is Nil" in {
    List.setHead(Nil, 7) shouldEqual List(7)
  }

  it should "be Nil if the tail is Nil and head is Nothing" in {
    List.setHead(Nil, ()) shouldEqual Nil
  }

  it should "be the tail if the head is Nothing" in {
    List.setHead(List(3, 4), ()) shouldEqual List(3, 4)
  }

  "drop" should "be the list with n elements removed from the front" in {
    List.drop(List(1, 2, 3, 4, 5), 2) shouldEqual List(3, 4, 5)
  }

  it should "be Nil when n is greater than the length of the list" in {
    List.drop(List(1, 2), 3) shouldEqual Nil
  }

  it should "be Nil when n is equal to the length of the list" in {
    List.drop(List(1, 2), 2) shouldEqual Nil
  }

  "drop while" should "be the list of elements after and including the first element that does not match the predicate" in {
    List.dropWhile(List(1, 2, 3), (x: Int) => x < 2) shouldEqual List(2, 3)
  }

  it should "be Nil when the list is empty" in {
    List.dropWhile(List(), (x: Int) => x < 2) shouldEqual Nil
  }

  it should "be the entire list when the predicate does not match the first element" in {
    List.dropWhile(List(1, 2, 3), (x: Int) => x < 0) shouldEqual List(1, 2, 3)
  }

  "init" should "return a list with all but the last item" in {
    List.init(List(1, 2, 3)) shouldEqual List(1, 2)
  }

  "length" should "return the length of the list" in {
    List.length(List(1, 2, 3)) shouldEqual 3
  }

  it should "be zero when the list is empty" in {
    List.length(List()) shouldEqual 0
  }

  "foldLeft" should "reduce from right to left" in {
    List.foldLeft(List(1, 2, 3), 0)((a: Int, b: Int) => {
      println("a + b = " + (a + b))
      a + b
    }) shouldEqual 6
  }
}
