package ch2

import org.scalatest._

class ComposeSpec extends FlatSpec with Matchers {
  "compose" should "return a function that calls the first argument with the result of the second" in {
    val composedFunction = Compose.compose((b: Int) => (b + 3), (a: Int) => (a * 4))
    composedFunction(2) shouldEqual 11
  }
}
