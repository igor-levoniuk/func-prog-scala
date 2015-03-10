package my.exercises.chapter2

import org.scalatest.{ShouldMatchers, WordSpec}


class Chapter2Test extends WordSpec with ShouldMatchers {

  "fib function" should {
    "return correct Fibonacci number" in {
      fib(0) shouldBe 0
      fib(1) shouldBe 1
      fib(2) shouldBe 1
      fib(3) shouldBe 2
      fib(4) shouldBe 3
      fib(5) shouldBe 5
      fib(6) shouldBe 8
      fib(7) shouldBe 13
      fib(8) shouldBe 21
      fib(9) shouldBe 34
      fib(10) shouldBe 55
    }
  }

  "isSorted function" should {
    "check that predicate holds" in {
      isSorted[String](Array("foo", "barr", "bazzz"), _.length < _.length) shouldBe true
      isSorted[String](Array("foo", "barr", "bazzz"), _.length > _.length) shouldBe false
      isSorted[String](Array("foo", "bar", "baz"), _.length == _.length) shouldBe true
      isSorted[Int](Array(1, 2, 5, 100), _ < _) shouldBe true
      isSorted[Int](Array(1, 2, 5, 100), _ > _) shouldBe false
      isSorted[Int](Array(1, 2, 5, 100), _ == _) shouldBe false
      isSorted[Int](Array(1, 1, 1, 1), _ == _) shouldBe true
    }
  }

}
