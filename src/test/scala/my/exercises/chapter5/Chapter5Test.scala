package my.exercises.chapter5

import org.scalatest.{ShouldMatchers, WordSpec}

class Chapter5Test extends WordSpec with ShouldMatchers {

  def aBomb[A]: A = throw new RuntimeException

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
        Empty.headOptionFR shouldBe None
      }
      "called on non-empty Stream" in {
        Cons(() => 42, () => Empty).headOption shouldBe Some(42)
        Cons(() => 42, () => Empty).headOptionFR shouldBe Some(42)
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
      cons(aBomb, aBomb).getClass shouldBe classOf[Cons[_]]

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

      Stream.empty.takeUnfold(1000) shouldBe Stream.empty
      Stream(1, 2, 3).takeUnfold(-1).toList shouldBe List.empty
      Stream(1, 2, 3).takeUnfold(0).toList shouldBe List.empty
      Stream(1, 2, 3).takeUnfold(3).toList shouldBe List(1, 2, 3)
      Stream(1, 2, 3).takeUnfold(5).toList shouldBe List(1, 2, 3)
      Stream(1, 2, 3, 4, 5).takeUnfold(2).toList shouldBe List(1, 2)
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

      Stream(true, true, false, true).takeWhileFR(identity).toList shouldBe List(true, true)
      Stream("foo", "barrr", "bazzz").takeWhileFR(_.length > 3) shouldBe Empty
      Stream("foo", "bar", "bz").takeWhileFR(_.length >= 3).toList shouldBe List("foo", "bar")

      Stream(true, true, false, true).takeWhileUnfold(identity).toList shouldBe List(true, true)
      Stream("foo", "barrr", "bazzz").takeWhileUnfold(_.length > 3) shouldBe Empty
      Stream("foo", "bar", "bz").takeWhileUnfold(_.length >= 3).toList shouldBe List("foo", "bar")
    }
    "have foldRight method which folds the Stream with the given function starting from the end" in {
      Stream.empty[Int].foldRight(0)(_ + _) shouldBe 0
      Stream(1, 2, 3, 4, 5).foldRight(0)(_ + _) shouldBe 15
      Stream.empty[Int].foldRight(1)(_ * _) shouldBe 1
      Stream(1, 2, 3, 4, 5).foldRight(1)(_ * _) shouldBe 120
      Stream("S", "c", "a").foldRight("la")(_ + _) shouldBe "Scala"
    }
    "have exists method which checks whether Stream contains elements matching a predicate" in {
      Stream.empty[Int].exists(_.isInstanceOf[Int]) shouldBe false
      Stream(1, 2, 3, 4).exists(_ >= 4) shouldBe true
      Stream(1, 2, 3, 4).exists(_ > 4) shouldBe false
      Stream("foo", "bar", "baz").exists(_.startsWith("f")) shouldBe true
      Stream("foo", "bar", "baz").exists(_.startsWith("z")) shouldBe false
      Stream(false, true, false).exists(identity) shouldBe true
      Stream(false, false, false).exists(identity) shouldBe false
    }
    "have forAll method which checks if all elemeths of a Stream match given predicate" in {
      Stream.empty[Int].forAll(_ > 1000) shouldBe true
      Stream(1, 2, 3, 4).forAll(_ <= 4) shouldBe true
      Stream(1, 2, 3, 4).forAll(_ < 4) shouldBe false
      Stream("foo", "bar", "baz").forAll(_.length == 3) shouldBe true
      Stream("foo", "bar", "baz").forAll(_.startsWith("b")) shouldBe false
      Stream(true, true, true).forAll(identity) shouldBe true
      Stream(false, true, true).forAll(identity) shouldBe false
    }
    "have map method which" should {
      "be lazy" in {
        cons[Int](aBomb, Empty).map(_ * 2)
      }
      "converts elements with given function" in {
        Stream.empty[Int].map(_ * 2).toList shouldBe List.empty[Int]
        Stream(1, 2, 3).map(_ + 1).toList shouldBe List(2, 3, 4)
        Stream(1, 2, 3).map(_.toString).toList shouldBe List("1", "2", "3")
        Stream("foo", "bar", "baz").map(_.charAt(2)).toList shouldBe List('o', 'r', 'z')

        Stream.empty[Int].mapUnfold(_ * 2).toList shouldBe List.empty[Int]
        Stream(1, 2, 3).mapUnfold(_ + 1).toList shouldBe List(2, 3, 4)
        Stream(1, 2, 3).mapUnfold(_.toString).toList shouldBe List("1", "2", "3")
        Stream("foo", "bar", "baz").mapUnfold(_.charAt(2)).toList shouldBe List('o', 'r', 'z')
      }
    }
    "have filter which" should {
      "be lazy" in {
        intercept[RuntimeException] { cons[Int](aBomb, Empty).filter(_ > 0) }
        intercept[RuntimeException] { cons(0, cons[Int](aBomb, Empty)).filter(_ > 0) }
        cons(1, cons[Int](aBomb, Empty)).filter(_ > 0)
      }
      "remove elements which doesn't match given predicate" in {
        Stream.empty[Int].filter(_ == 1).toList shouldBe List.empty[Int]
        Stream(1, 2, 3).filter(_ > 0).toList shouldBe List(1, 2, 3)
        Stream(1, 2, 3, 4, 5).filter(_ <= 3).toList shouldBe List(1, 2, 3)
        Stream(true, false, true, false).filter(x => x).toList shouldBe List(true, true)
        Stream("foo", "bar", "baz").filter(_.startsWith("b")).toList shouldBe List("bar", "baz")
      }
    }
    "have lazy append method which adds all elements of other Stream to the end of this stream" in {
      Stream.empty.append(Stream(42)).toList shouldBe List(42)
      Stream(1, 2, 3).append(Stream(4, 5)).toList shouldBe List(1, 2, 3, 4, 5)
      Stream("foo", "bar").append(Stream("baz", "banana")).toList shouldBe List("foo", "bar", "baz", "banana")
    }
    "have flatMap method which converts each element to Stream with given function and then merges sub-results" in {
      Stream.empty[Int].flatMap(n => Stream(n)).toList shouldBe List.empty[Int]
      Stream(1, 2, 3).flatMap(n => Stream(n)).toList shouldBe List(1, 2, 3)
      Stream("foo", "bar").flatMap(s => Stream(s: _*)).toList shouldBe List('f', 'o', 'o', 'b', 'a', 'r')
      Stream(Some(1), None, Some(42), None, Some(7))
        .flatMap(_.map(n => Stream(n)).getOrElse(Stream.empty)).toList shouldBe List(1, 42, 7)
    }
    "have method to generate Stream of integer ones" in {
      Stream.ones.take(3).toList shouldBe List(1, 1, 1)
      Stream.ones.take(100).forAll(_ == 1)

      Stream.onesUnfold.take(3).toList shouldBe List(1, 1, 1)
      Stream.onesUnfold.take(100).forAll(_ == 1)
    }
    "have method to generate Stream of constant values" in {
      Stream.constant(42).take(3).toList shouldBe List(42, 42, 42)
      Stream.constant("foo").take(100).forAll(_ == "foo")

      Stream.constantUnfold(42).take(3).toList shouldBe List(42, 42, 42)
      Stream.constantUnfold("foo").take(100).forAll(_ == "foo")
    }
    "have method to generate stream of natural numbers" in {
      Stream.from(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)
      Stream.from(-1).take(3).toList shouldBe List(-1, 0, 1)
      Stream.from(Int.MaxValue).take(1).head shouldBe Int.MaxValue

      Stream.fromUnfold(1).take(5).toList shouldBe List(1, 2, 3, 4, 5)
      Stream.fromUnfold(-1).take(3).toList shouldBe List(-1, 0, 1)
      Stream.fromUnfold(Int.MaxValue).take(1).head shouldBe Int.MaxValue
    }
    "have method generating Fibonacci numbers" in {
      Stream.fibs.take(10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)

      Stream.fibsUnfold.take(10).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }
    "have unfold method for constructing Streams" in {
      Stream.unfold(true)(_ => None) shouldBe Stream.empty
      Stream.unfold(true)(b => if (b) Some(("foo", false)) else Some(("bar", true)))
        .take(5).toList shouldBe List("foo", "bar", "foo", "bar", "foo")
      Stream.unfold(1)(x => if (x <= 5) Some((x, x + 1)) else None).toList shouldBe List(1, 2, 3, 4, 5)
    }
    "have method to zip two Streams together using provided function for combining elements" in {
      Stream.empty.zipWith(Stream.empty)((_: Nothing, _: Nothing) => aBomb) shouldBe Stream.empty
      Stream.empty[Int].zipWith(Stream(1, 2))((_, _) => aBomb).toList shouldBe List(1, 2)
      Stream(1, 2).zipWith(Stream.empty)((_, _) => aBomb).toList shouldBe List(1, 2)
      Stream(1, 2, 3).zipWith(Stream(3, 2, 1))(_ + _).toList shouldBe List(4, 4, 4)
      Stream("foo", "bar", "baz").zipWith(Stream("baz", "bar", "foo"))(_ + _)
        .toList shouldBe List("foobaz", "barbar", "bazfoo")
      Stream(1, 2, 3, 4, 5).zipWith(Stream(3, 2, 1))(_ + _).toList shouldBe List(4, 4, 4, 4, 5)
      Stream(1, 2, 3).zipWith(Stream(3, 2, 1, 0, -1))(_ + _).toList shouldBe List(4, 4, 4, 0, -1)
    }
    "have method to zip Streams (potetially of different legth) into Stream of Pairs" in {
      Stream.empty.zipAll(Stream.empty) shouldBe Stream.empty
      Stream(1, 2).zipAll(Stream.empty).toList shouldBe List((Some(1), None), (Some(2), None))
      Stream.empty.zipAll(Stream(1, 2)).toList shouldBe List((None, Some(1)), (None, Some(2)))

      Stream("foo", "bar", "baz").zipAll(Stream("baz", "bar", "foo")).toList shouldBe
        List((Some("foo"), Some("baz")), (Some("bar"), Some("bar")), (Some("baz"), Some("foo")))

      Stream(1, 2, 3, 4, 5).zipAll(Stream(3, 2, 1)).toList shouldBe
        List((Some(1), Some(3)), (Some(2), Some(2)), (Some(3), Some(1)), (Some(4), None), (Some(5), None))

      Stream(1, 2, 3).zipAll(Stream(3, 2, 1, 0, -1)).toList shouldBe
        List((Some(1), Some(3)), (Some(2), Some(2)), (Some(3), Some(1)), (None, Some(0)), (None, Some(-1)))
    }
    "have method indicating whether this Stream starts with other Stream" in {
      Stream.empty.startsWith(Stream(42)) shouldBe false
      Stream.empty.startsWith(Stream.empty) shouldBe true

      Stream(1, 2, 3).startsWith(Stream(1, 2, 3)) shouldBe true
      Stream(1, 2, 3).startsWith(Stream(1, 2)) shouldBe true
      Stream(1, 2, 3).startsWith(Stream(1)) shouldBe true

      Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)) shouldBe false
      Stream(1, 2, 3).startsWith(Stream(1, 2, 4)) shouldBe false
      Stream(1, 2, 3).startsWith(Stream(1, 4)) shouldBe false
      Stream(1, 2, 3).startsWith(Stream(2, 3)) shouldBe false
      Stream(1, 2, 3).startsWith(Stream(2)) shouldBe false
    }
    "have method producing all possible tails of current stream" in {
      Stream.empty.tails shouldBe Stream.empty
      Stream(1).tails.toList.map(_.toList) shouldBe List(List(1))
      Stream(1, 2, 3).tails.toList.map(_.toList) shouldBe List(List(1, 2, 3), List(2, 3), List(3))
    }
    "have method indicating whether specified Stream is subsequence of this" in {
      Stream(1, 2, 3, 4).hasSubsequence(Stream(2, 3)) shouldBe true
      Stream(1, 2, 3).hasSubsequence(Stream(1, 2, 3)) shouldBe true
      Stream(1, 2, 3).hasSubsequence(Stream(1, 2)) shouldBe true
      Stream(1, 2, 3).hasSubsequence(Stream(1)) shouldBe true
      Stream(1, 2, 3).hasSubsequence(Stream(2)) shouldBe true
      Stream(1, 2, 3).hasSubsequence(Stream(3)) shouldBe true

      Stream.empty.hasSubsequence(Stream(1)) shouldBe false
      Stream(1, 2, 3, 4).hasSubsequence(Stream(1, 2, 4)) shouldBe false
      Stream(1, 2, 3, 4).hasSubsequence(Stream(3, 2)) shouldBe false
    }
    "have scanRight method which" should {
      "be lazy" in {
        val timeBomb = cons(1, cons(2, cons(3, cons(aBomb, Empty)))).scanRight(0)(_ + _)
        intercept[RuntimeException](timeBomb.toList)
      }
      "performs fold and returns Stream of intermidiate results" in {
        Stream.empty[Int].scanRight(42)(_ + _).toList shouldBe List(42)
        Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldBe List(6, 5, 3, 0)
        Stream("foo", "bar", "baz").scanRight("")(_ + _).toList shouldBe List("foobarbaz", "barbaz", "baz", "")
      }
    }
  }

}
