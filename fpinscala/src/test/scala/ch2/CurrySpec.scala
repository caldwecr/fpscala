package ch2

import org.scalatest._

class CurrySpec extends FlatSpec with Matchers {
  "curry" should "work for addition" in {
    Curry.curry((x: Int, y: Int) => x + y)(4)(5) shouldEqual 9
  }
}
