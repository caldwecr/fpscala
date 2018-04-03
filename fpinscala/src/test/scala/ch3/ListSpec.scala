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

  "sumLeft" should "equal to the sum of the elements in the List" in {
    List.sumLeft(List(444, 555, 666)) shouldEqual 444 + 555 + 666
  }

  "productLeft" should "equal to the product of the elements in the list" in {
    List.productLeft(List(2, 3, 4)) shouldEqual 2 * 3 * 4
  }

  it should "be zero if any elements in the list are 0" in {
    List.productLeft(List(2, 0, 4)) shouldEqual 0
  }

  "lengthLeft" should "equal to the number of items in the list" in {
    List.lengthLeft(List("foo", "bar", "baz")) shouldEqual 3
  }

  "reverse" should "return the list with the elements in reverse order" in {
    List.reverse(List("foo", "bar", "baz")) shouldEqual List("baz", "bar", "foo")
  }

  "foldLeftUsingRight" should "be a left fold" in {
    List.foldLeftUsingRight(List(1, 2, 3), Nil: List[Int])((acc, curr) => Cons(curr, acc)) shouldEqual List(3, 2, 1)
  }

  "foldRightUsingLeft" should "be a right fold" in {
    List.foldRightUsingLeft(List(1, 2, 3), Nil: List[Int])((curr, acc) => Cons(curr, acc)) shouldEqual List(1, 2, 3)
  }

  "append" should "add the item to the end of the List" in {
    List.append(List("foo", "bar"), "baz") shouldEqual List("foo", "bar", "baz")
  }

  "flatten" should "flatten a List of Lists into a List" in {
    List.flatten(List(
      List(1, 2, 3),
      List(10, 20, 30, 40),
      List(100, 200, 300, 400, 500)
    )) shouldEqual List(1, 2, 3, 10, 20, 30, 40, 100, 200, 300, 400, 500)
  }

  "concat" should "append the items from the second list to the first list" in {
    List.concat(List(1, 3, 4), List(10, 100, 1000)) shouldEqual List(1, 3, 4, 10, 100, 1000)
  }

  "increment" should "add 1 to every item in the List" in {
    List.increment(List(7, 11, 13)) shouldEqual List(8, 12, 14)
  }

  "doubleAsString" should "convert all the elements to string representations" in {
    List.doubleAsString(List(3.5, -10.2, 99.0012)) shouldEqual List("3.5", "-10.2", "99.0012")
  }

  "map" should "apply its function to all elements in the List" in {
    List.map(List(5, 10, 25))(_ * 5) shouldEqual List(25, 50, 125)
  }

  "filter" should "remove elements that do not satisfy the given predicate" in {
    List.filter(List(5, 10, 25, 99))(_ > 11) shouldEqual List(25, 99)
  }

  "flatMap" should "flatten the results of the passed function" in {
    List.flatMap(List(1, 2 ,3))(i => List(i, i)) shouldEqual List(1, 1, 2, 2, 3, 3)
  }

  "filterByFlatMap" should "remove elements that do not satisfy the given predicate" in {
    List.filterByFlatMap(List(5, 10, 25, 99))(_ > 11) shouldEqual List(25, 99)
  }

  "sumCorresponding" should "sum the corresponding elements in two lists" in {
    List.sumCorresponding(List(1, 2, 3), List(4, 5, 6)) shouldEqual List(5, 7, 9)
  }

  "sumCorrespondingToo" should "sum the corresponding elements in two lists" in {
    List.sumCorrespondingToo(List(1, 2, 3), List(4, 5, 6)) shouldEqual List(5, 7, 9)
  }

  "headAsList" should "return the empty list when the input List is empty" in {
    List.headAsList(Nil: List[Int]) shouldEqual List()
  }

  "headAsList" should "return a List with just the head when the input List is not empty" in {
    List.headAsList(List(1, 2, 3)) shouldEqual List(1)
  }

  "zipWith" should "apply the function to each corresponding pair" in {
    List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ * _) shouldEqual List(4, 10, 18)
  }

  "zipWithToo" should "apply the function to each corresponding pair" in {
    List.zipWithToo(List(1, 2, 3), List(4, 5, 6))(_ * _) shouldEqual List(4, 10, 18)
  }

  "zipAndCheckEquality" should "be true for all equal corresponding items and false otherwise" in {
    List.zipAndCheckEquality(List(1, 2, 3, 4), List(2, 2, 2, 4)) shouldEqual List(false, true, false, true)
  }

  "startWith" should "return true when the list begins with the prefix" in {
    List.startsWith(List("cat", "mouse", "dog"), List("cat", "mouse")) shouldBe true
  }

  it should "return false when the list does not being with the entire prefix" in {
    List.startsWith(List("cat", "mouse", "dog"), List("cat", "dog")) shouldBe false
  }

  "hasSubsequence" should "return true when the sup contains the sub at the beginning" in {
    List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) shouldBe true
  }

  it should "return true when the sup contains the sub in the middle" in {
    List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) shouldBe true
  }

  it should "return true when the sup contains the sub at the end" in {
    List.hasSubsequence(List(1, 2, 3, 4), List(4)) shouldBe true
  }

  it should "return false the the sup does not contain the sub" in {
    List.hasSubsequence(List(1, 2, 3, 4), List(3, 2)) shouldBe false
  }
}
