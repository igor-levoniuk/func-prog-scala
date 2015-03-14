package my.exercises.chapter3

import org.scalatest.{ShouldMatchers, WordSpec}

class Chapter3Test extends WordSpec with ShouldMatchers {

  "Expression should yield 3" in {
    val res = List(1, 2, 3, 4, 5) match {
      case  Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    res shouldBe 3
  }

  "tail" when {
    "list is empty" should {
      "return empty list" in {
        List.tail(Nil) shouldBe Nil
      }
    }
    "list is non-empty" should {
      "return list without head element" in {
        List.tail(List(1, 2, 3)) shouldBe List(2, 3)
      }
    }
  }

  "tailOption" when {
    "list is empty" should {
      "return empty option" in {
        List.tailOption(Nil) shouldBe None
      }
    }
    "list is non-empty" should {
      "return option containing list without head element" in {
        List.tailOption(List(1, 2, 3)) shouldBe Some(List(2, 3))
        List.tailOption(List(1, 2)) shouldBe Some(List(2))
        List.tailOption(List(1)) shouldBe Some(Nil)
      }
    }
  }

  "setHead" when {
    "called on empty list" should {
      "return one element list containing head element specified" in {
        List.setHead(42, Nil) shouldBe List(42)
      }
    }
    "called on non-empty list" should {
      "return list with specified element instead of previous head element of the list" in {
        List.setHead(42, List(1, 2, 3)) shouldBe List(42, 2, 3)
      }
    }
  }

  "drop" when {
    "called on empty list" should {
      "return empty list" in {
        List.drop(Nil, 100000) shouldBe Nil
        List.drop(Nil, 0) shouldBe Nil
      }
    }
    "called on non-empty list" should {
      "return list with first n elements removed" in {
        List.drop(List(1, 2, 3), 3) shouldBe Nil
        List.drop(List(1, 2, 3), 1) shouldBe List(2, 3)
        List.drop(List(1, 2, 3), 0) shouldBe List(1, 2, 3)
      }
    }
  }

  "dropWhile" when {
    "called on empty list" should {
      "return empty list" in {
        List.dropWhile(Nil)(_ => true) shouldBe Nil
        List.dropWhile(Nil)(_ => false) shouldBe Nil
      }
    }
    "called on non-empty list" should {
      "return list with first n elements matching predicate removed" in {
        List.dropWhile(List(1, 2, 3))(_ => true) shouldBe Nil
        List.dropWhile(List(1, 2, 3))(_ => false) shouldBe List(1, 2, 3)
        List.dropWhile(List(1, 2, 3))(_ < 3) shouldBe List(3)
        List.dropWhile(List(1, 1, 1, 2, 1))(_ <= 1) shouldBe List(2, 1)
      }
    }
  }

  "init" when {
    "called on empty list" should {
      "return empty list" in {
        List.init(Nil) shouldBe Nil
      }
    }
    "called on non-empty list" should {
      "return list with first n elements matching predicate removed" in {
        List.init(List(1)) shouldBe Nil
        List.init(List(1, 2, 3, 4, 5)) shouldBe List(1, 2, 3, 4)
      }
    }
  }

  "smartProduct" should {
    "return zero upon first encountered zero element in the list" in {
      List.iterationCountingProduct(List(1, 2, 3, 0, 4, 5)) shouldBe (0, 4)
      List.iterationCountingProduct(List(0, 1, 2, 3, 4, 5)) shouldBe (0, 1)
    }
  }

  "foldRight" when {
    "folding list with cons" should {
      "work as list constructor" in {
        List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3)
        List.foldRightUsingFoldLeft(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(1, 2, 3)
      }
    }
    "very large list supplied" should {
      "fail with stack overflow error" in {
        intercept[StackOverflowError] {
          List.foldRight(List[Int](1 to 2000 : _*), 0)(_ + _)
        }
      }
    }
  }

  "length function" should {
    "return the length of the list specified" in {
      List.length(Nil) shouldBe 0
      List.length(List(1)) shouldBe 1
      List.length(List(1, 2, 3, 4, 5)) shouldBe 5
    }
  }

  "foldLeft" should {
    "be able to calculate sum, product and length of a list just as foldRight" in {
      val xs = List[Int](1 to 101: _*)
      val ds = List[Double]((1 to 23).map(_.toDouble): _*)
      List.sum(xs) shouldEqual List.sumFoldLeft(xs)
      List.product(ds) shouldEqual List.productFoldLeft(ds)
      List.length(xs) shouldEqual List.lengthFoldLeft(xs)
      List.sum(xs) shouldEqual List.foldRightUsingFoldLeft(xs, 0)(_ + _)
      List.product(ds) shouldEqual List.foldRightUsingFoldLeft(ds, 1.0)(_ * _)
      List.length(xs) shouldEqual List.foldRightUsingFoldLeft(xs, 0)((_, b) => b + 1)
    }
    "not fall with stack overflow" in {
      List.foldLeft(List[Int](1 to 5000 : _*), 0)(_ + _)
    }
    "reverse elements when folding list with cons" in {
      List.foldLeft(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe List(3, 2, 1)
    }
  }

  "reverse" should {
    "return the list with elements in reversed order" in {
      List.reverse(List(1, 2, 3, 4, 5)) shouldBe List(5, 4, 3, 2, 1)
    }
  }

  "append" when {
    "should add element to the end of the list" in {
      List.append(Nil, 42) shouldBe List(42)
      List.append(List(1, 2), 3) shouldBe List(1, 2, 3)
    }
  }

  "concat" should {
    "concatenate all provided list into single one, preserving the ordering" in {
      List.concat(List(List(1, 2), Nil, List(3))) shouldBe List(1, 2, 3)
      List.concat(List(Nil, List(1, 2, 3), List(4, 5), List(6, 7), Nil)) shouldBe List(1, 2, 3, 4, 5, 6, 7)
      List.concat(Nil) shouldBe Nil
    }
  }

}
