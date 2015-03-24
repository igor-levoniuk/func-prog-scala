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
      List.flatten(List(List(1, 2), Nil, List(3))) shouldBe List(1, 2, 3)
      List.flatten(List(Nil, List(1, 2, 3), List(4, 5), List(6, 7), Nil)) shouldBe List(1, 2, 3, 4, 5, 6, 7)
      List.flatten(Nil) shouldBe Nil
    }
  }

  "plusOne" should {
    "increase each element of a list by 1" in {
      List.plusOne(Nil) shouldBe Nil
      List.plusOne(List(1, 2, 3)) shouldBe List(2, 3, 4)
    }
  }

  "asStrings" should {
    "convert all elements of a list into strings" in {
      List.asStrings(Nil) shouldBe Nil
      List.asStrings(List(1, 2, 3)) shouldBe List("1", "2", "3")
      List.asStrings(List("foo", "bar", "baz")) shouldBe List("foo", "bar", "baz")
    }
  }

  "map" should {
    "convert elements using function supplied" in {
      List.map(Nil)(_ => throw new RuntimeException) shouldBe Nil
      List.map(List(1, 2, 3))(_ + 1) shouldBe List(2, 3, 4)
      List.map(List(1, 2, 3))(_.toString) shouldBe List("1", "2", "3")
      List.map(List("foo", "bar", "baz"))(_ + "s") shouldBe List("foos", "bars", "bazs")

      List.tailRecursiveMap(Nil)(_ => throw new RuntimeException) shouldBe Nil
      List.tailRecursiveMap(List(1, 2, 3))(_ + 1) shouldBe List(2, 3, 4)
      List.tailRecursiveMap(List(1, 2, 3))(_.toString) shouldBe List("1", "2", "3")
      List.tailRecursiveMap(List("foo", "bar", "baz"))(_ + "s") shouldBe List("foos", "bars", "bazs")
    }
  }

  "filter" should {
    "return list containing only those elements which match predicate" in {
      List.filter(List(1, 2, 3, 4, 5))(_ % 2 == 0) shouldBe List(2, 4)
      List.filter(List(1, 2, 1, 3, 1, 4, 1, 5, 1))(_ >= 2) shouldBe List(2, 3, 4, 5)

      List.tailRecursiveFilter(List(1, 2, 3, 4, 5))(_ % 2 == 0) shouldBe List(2, 4)
      List.tailRecursiveFilter(List(1, 2, 1, 3, 1, 4, 1, 5, 1))(_ >= 2) shouldBe List(2, 3, 4, 5)

      List.filterUsingFlatMap(List(1, 2, 3, 4, 5))(_ % 2 == 0) shouldBe List(2, 4)
      List.filterUsingFlatMap(List(1, 2, 1, 3, 1, 4, 1, 5, 1))(_ >= 2) shouldBe List(2, 3, 4, 5)
    }
  }

  "flatMap" should {
    "transform each element into list with supplied function and concat (flatten) resulting lists" in {
      List.flatMap(Nil)(_ => throw new RuntimeException) shouldBe Nil
      List.flatMap(List(1, 2, 3))(List(_)) shouldBe List(1, 2, 3)
      List.flatMap(List(1, 2, 3))(x => List(x, x)) shouldBe List(1, 1, 2, 2, 3, 3)
      List.flatMap(List(1, 2, 3))(x => List.map(List(1, 2, 3))(_ * x)) shouldBe List(1, 2, 3, 2, 4, 6, 3, 6, 9)

      List.tailRecursiveFlatMap(Nil)(_ => throw new RuntimeException) shouldBe Nil
      List.tailRecursiveFlatMap(List(1, 2, 3))(List(_)) shouldBe List(1, 2, 3)
      List.tailRecursiveFlatMap(List(1, 2, 3))(x => List(x, x)) shouldBe List(1, 1, 2, 2, 3, 3)
      List.tailRecursiveFlatMap(List(1, 2, 3))(x => List.map(List(1, 2, 3))(_ * x)) shouldBe List(1, 2, 3, 2, 4, 6, 3, 6, 9)
    }
  }

  "sumLists" should {
    "add elements of two integer lists" in {
      List.sumLists(List(1, 2, 3), Nil) shouldBe List(1, 2, 3)
      List.sumLists(List(1, 2, 3), List(1, 2, 3)) shouldBe List(2, 4, 6)
      List.sumLists(List(1, 2), List(1)) shouldBe List(2, 2)
    }
  }

  "zipWith" should {
    "combine two arbitrary lists into single one using provided folding function" in {
      List.zipWith(List(1, 2, 3), Nil)(_ + _) shouldBe List(1, 2, 3)
      List.zipWith(List(1, 2, 3), List(1, 2, 3))(_ + _) shouldBe List(2, 4, 6)
      List.zipWith(List(1, 2), List(1))(_ + _) shouldBe List(2, 2)
      List.zipWith(List("foo", "bar", "baz"), List("baz", "bar", "foo"))(_ + _) shouldBe List("foobaz", "barbar", "bazfoo")
      List.zipWith(List(true, true, false, false), List(true, false, true, false))(_ && _) shouldBe List(true, false, false, false)
    }
  }

  "hasSubsequence" should {
    "indicate whether one list is a sub-list of other" in {
      List.hasSubsequence(Nil, Nil) shouldBe true
      List.hasSubsequence(List(1, 2, 3), Nil) shouldBe true
      List.hasSubsequence(List(1, 2, 3), List(1, 2, 3)) shouldBe true
      List.hasSubsequence(List(1, 2), List(2, 1)) shouldBe false
      List.hasSubsequence(List("foo", "bar", "baz"), List("foo")) shouldBe true
      List.hasSubsequence(List("foo", "bar", "baz"), List("foo", "bar")) shouldBe true
      List.hasSubsequence(List("foo", "bar", "baz"), List("foo", "baz")) shouldBe false
    }
  }


  "size" should {
    "calculate size of a tree" in {
      Tree.size(Leaf(42)) shouldBe 1
      Tree.size(Node(Leaf(1), Leaf(2))) shouldBe 3
      Tree.size(Node(Leaf("1"), Leaf(true))) shouldBe 3
      Tree.size(Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4)))) shouldBe 7
    }
  }

  "maximum" should {
    "return max element of an integer tree" in {
      Tree.maximum(Leaf(-1)) shouldBe -1
      Tree.maximum(Node(Leaf(1), Leaf(2))) shouldBe 2
      Tree.maximum(Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4)))) shouldBe 4
      Tree.maximum(Node(Node(Leaf(4), Leaf(2)), Node(Leaf(3), Leaf(1)))) shouldBe 4
      Tree.maximum(Node(Node(Leaf(1), Leaf(4)), Node(Leaf(3), Leaf(2)))) shouldBe 4
      Tree.maximum(Node(Node(Leaf(1), Leaf(3)), Node(Leaf(4), Leaf(2)))) shouldBe 4
    }
  }

  "depth" should {
    "calculate max depth of a tree" in {
      Tree.depth(Leaf(42)) shouldBe 1
      Tree.depth(Node(Leaf(1), Leaf(2))) shouldBe 2
      Tree.depth(Node(Leaf("1"), Leaf(true))) shouldBe 2
      Tree.depth(Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4)))) shouldBe 3
      Tree.depth(Node(Leaf(1), Node(Leaf(2), Node(Leaf(3), Node(Leaf(4), Leaf(5)))))) shouldBe 5
      Tree.depth(Node(Node(Node(Node(Leaf(5), Leaf(4)), Leaf(3)), Leaf(2)), Leaf(1))) shouldBe 5
    }
  }

  "map" should {
    "apply specified function over each element of the tree, preserving the structure" in {
      val t = Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4)))

      Tree.map(t)(_.toString) shouldBe Node(Node(Leaf("1"), Leaf("2")), Node(Leaf("3"), Leaf("4")))
      Tree.map(t)(_ + 1) shouldBe Node(Node(Leaf(2), Leaf(3)), Node(Leaf(4), Leaf(5)))
      Tree.map(t)(x => if (x % 2 == 0) "even" else "odd") shouldBe Node(Node(Leaf("odd"), Leaf("even")), Node(Leaf("odd"), Leaf("even")))
    }
  }

}
