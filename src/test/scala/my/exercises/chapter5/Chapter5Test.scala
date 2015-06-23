package my.exercises.chapter5

import org.scalatest.{ShouldMatchers, WordSpec}

class Chapter5Test extends WordSpec with ShouldMatchers {

  import Stream._

  "Stream" should {
    "have a function returning head of a Stream such that" when {
      "called on empty Stream" in {
        intercept[NoSuchElementException] { Empty.head }
      }
      "called on non-empty Stream" in {
        Cons(() => 42, () => Empty).head shouldBe 42
      }
    }
    "have a function returning Option of Stream head such that" when {
      "called on empty Stream" in {
        Empty.headOption shouldBe None
      }
      "called on non-empty Stream" in {
        Cons(() => 42, () => Empty).headOption shouldBe Some(42)
      }
    }
    "have a function returning tail of a Stream such that" when {
      "called on empty Stream" in {
        intercept[NoSuchElementException] { Empty.tail }
      }
      "called on non-empty Stream" in {
        Cons(() => 42, () => Empty).tail shouldBe Empty
      }
    }
    "have non-strict smart constructor which supports memoization" in {
      cons(throw new RuntimeException, throw new RuntimeException).getClass shouldBe classOf[Cons[_]]

      var a = 3
      def thunkWithSideEffect: Int = { a += 1; a }
      val s1 =  cons(thunkWithSideEffect, cons(thunkWithSideEffect, Empty))
      s1.head shouldBe s1.head
      s1.tail shouldBe s1.tail
    }
    "have factory method for building empty Stream" in {
      Stream.empty shouldBe Empty
      Stream.empty shouldBe Stream.empty
    }
    "have factory method for building a Stream from sequence of elements" in {
      Stream() shouldBe Empty

      val s1 = Stream(1)
      s1.head shouldBe 1
      s1.tail shouldBe Empty

      val s2 = Stream(1, 2, 3, 4, 5)
      s2.head shouldBe 1
      s2.tail.head shouldBe 2
      s2.tail.tail.head shouldBe 3
      s2.tail.tail.tail.head shouldBe 4
      s2.tail.tail.tail.tail.head shouldBe 5
      s2.tail.tail.tail.tail.tail shouldBe Empty
    }
    "have method for converting it to a List" in {
      Stream.empty.toList shouldBe List.empty
      Stream(1).toList shouldBe List(1)
      Stream(1, 2, 3, 4, 5).toList shouldBe List(1, 2, 3, 4, 5)
    }
    "have method returning first n elements of a Stream" in {
      Stream.empty.take(1000) shouldBe Stream.empty
      Stream(1, 2, 3).take(-1).toList shouldBe List.empty
      Stream(1, 2, 3).take(0).toList shouldBe List.empty
      Stream(1, 2, 3).take(3).toList shouldBe List(1, 2, 3)
      Stream(1, 2, 3).take(5).toList shouldBe List(1, 2, 3)
      Stream(1, 2, 3, 4, 5).take(2).toList shouldBe List(1, 2)
    }
    "have method dropping first n elements of a Stream" in {
      Stream.empty.drop(1000) shouldBe Stream.empty
      Stream(1, 2, 3).drop(3) shouldBe Stream.empty
      Stream(1, 2, 3).drop(5) shouldBe Stream.empty
      Stream(1, 2, 3).drop(2).toList shouldBe List(3)
      Stream(1, 2, 3, 4, 5).drop(1).toList shouldBe List(2, 3, 4, 5)
      Stream(1, 2, 3).drop(0).toList shouldBe List(1, 2, 3)
      Stream(1, 2, 3).drop(-1).toList shouldBe List(1, 2, 3)

    }
    "have method returning all leading elements of a Stream which match predicate" in {
      Stream(true, true, false, true).takeWhile(identity).toList shouldBe List(true, true)
      Stream("foo", "barrr", "bazzz").takeWhile(_.length > 3) shouldBe Empty
      Stream("foo", "bar", "bz").takeWhile(_.length >= 3).toList shouldBe List("foo", "bar")
    }
  }

}
