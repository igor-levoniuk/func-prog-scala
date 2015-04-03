package my.exercises.chapter4

import org.scalatest.{ShouldMatchers, WordSpec}

class Chapter4Test extends WordSpec with ShouldMatchers {

  "Empty option" when {
    "trying to get its value" should {
      "throw NoSuchElementException" in {
        intercept[NoSuchElementException] {
          None.get
        }
      }
    }
    "trying to map/faltMap its value" should {
      "return empty option without evaluating mapping function" in {
        None.map(_ => throw new RuntimeException) shouldBe None
        None.flatMap(_ => throw new RuntimeException) shouldBe None
      }
    }
    "trying to filter it" should {
      "return empty option without ever applying specified predicate" in {
        None.filter(_ => throw new RuntimeException) shouldBe None
      }
    }
    "trying to get value using fallback value" should {
      "return provided fallback value" in {
        None.getOrElse(42) shouldBe 42
        None.orElse(None) shouldBe None
        None.orElse(Some(42)) shouldBe Some(42)
      }
    }
  }

  "Non-empty option" when {
    "trying to get its value" should {
      "return value" in {
        Some(42).get shouldBe 42
        Some("foo").get shouldBe "foo"
      }
    }
    "trying to map its value" should {
      "be converted to non-empty option of possibly different type" in {
        Some(42).map(_ + 1) shouldBe Some(43)
        Some("42").map(_.toInt) shouldBe Some(42)
      }
    }
    "trying to map its value" should {
      "be converted to possibly empty option of possibly different type" in {
        Some(42).flatMap(x => Some(x + 1)) shouldBe Some(43)
        Some("42").flatMap { x => try { Some(x.toInt) } catch { case e: Exception => None} } shouldBe Some(42)
        Some("foo").flatMap { x => try { Some(x.toInt) } catch { case e: Exception => None} } shouldBe None
      }
    }
    "trying to filter it" when {
      "value contained in option matches specified predicate" should {
        "return non-empty option of same value" in {
          Some("banana").filter(_ => true) shouldBe Some("banana")
          Some(42).filter(_ % 2 == 0) shouldBe Some(42)
          Some("foo").filter(_.length >= 3) shouldBe Some("foo")
        }
      }
      "value contained in option doesn't matches specified predicate" should {
        "return empty option" in {
          Some("banana").filter(_ => false) shouldBe None
          Some(42).filter(_ % 2 != 0) shouldBe None
          Some("foo").filter(_.length < 3) shouldBe None
        }
      }
    }
    "trying to get value using fallback value" should {
      "return value contained inside the option without evaluating fallback value" in {
        Some(42).getOrElse(throw new RuntimeException) shouldBe 42
        Some(42).getOrElse(43) shouldBe 42
        Some(42).orElse(throw new RuntimeException) shouldBe Some(42)
        Some(42).orElse(None) shouldBe Some(42)
        Some(42).orElse(Some(43)) shouldBe Some(42)
      }
    }
  }

  "Try" when {
    "provided function ends normally" should {
      "return option containing function result" in {
        Try(42) shouldBe Some(42)
        Try("foo" + "bar") shouldBe Some("foobar")
      }
    }
    "provided function throws an exception" should {
      "return empty option" in {
        Try(throw new RuntimeException) shouldBe None
        Try(42 / 0) shouldBe None
        Try(List.empty.head) shouldBe None
      }
    }
  }

  "mean of a sequence of numbers should be calculated" in {
    mean(Seq.empty) shouldBe None
    mean(Seq(1, 1, 1)) shouldBe Some(1.0)
    mean(Seq(1, 2, 3)) shouldBe Some(2.0)
    mean(Seq(100, 10, 52)) shouldBe Some(54.0)
  }

  "variance of a sequence of numbers should be calculated" in {
    variance(Seq.empty) shouldBe None
    variance(Seq(1, 1, 1)) shouldBe Some(0)
    variance(Seq(1, 2, 3, 2, 1)) shouldBe Some(0.56)
    variance(Seq(5, 6, 9, 7)) shouldBe Some(2.1875)
  }

  "map2" when {
    "either or both options are empty" should {
      "return empty option" in {
        map2(None, None)((_, _) => throw new RuntimeException) shouldBe None
        map2(Some(42), None)((_, _) => throw new RuntimeException) shouldBe None
        map2(None, Some(42))((_, _) => throw new RuntimeException) shouldBe None
      }
    }
    "both options are non-empty" should {
      "return non-empty option containing result of specified function" in {
        map2(Some(42), Some(1))(_ + _) shouldBe Some(43)
        map2(Some("foo"), Some("bar"))(_ + _) shouldBe Some("foobar")
        map2(Some("foo"), Some(42))(_ + _) shouldBe Some("foo42")
      }
    }
  }

  "sequence" when {
    "called on a list containing empty option(s)" should {
      "return empty option" in {
        sequence(List(None, None, None)) shouldBe None
        sequence(List(Some(42), Some("foo"), None)) shouldBe None
      }
    }
    "called on a list containing only non-empty options" should {
      "return List with options flattened (extracted options values)" in {
        sequence(List.empty) shouldBe Some(List.empty)
        sequence(List(Some(42), Some("foo"), Some(true))) shouldBe Some(List(42, "foo", true))
      }
    }
  }

  "traverse" when {
    "whole list can be converted to non-empty option with provided function" should {
      "return non-empty option on a list of flattened (extracted) option values" in {
        traverse(List(1, 2, 3, 4, 5))(x => Try(x.toString)) shouldBe Some(List("1", "2", "3", "4", "5"))
        traverse(List("1", "2", "3", "4", "5"))(x => Try(x.toInt)) shouldBe Some(List(1, 2, 3, 4, 5))
      }
    }
    "some elements of the list can't convrted (list will contain empty options)" should {
      "return empty option" in {
        traverse(List(1, 2, 3, 4, 5))(x => Try(x / (x - 3))) shouldBe None
        traverse(List("1", "2", "3", "banana", "5"))(x => Try(x.toInt)) shouldBe None
      }
    }
  }


  "right either" when {
    "trying to map its value" should {
      "return right either containing result of applying provided function" in {
        Right(42).map(_ + 1) shouldBe Right(43)
        Right(42).map(_.toString) shouldBe Right("42")
      }
    }
    "applying another operation to its value with flatMap" should {
      "return outcome of applying provided function which will be right or left either" in {
        val divByZeroError = Left("Division by zero")
        val oneDivX: Int => Either[String, Int] = x => if (x != 0) Right(1 / x) else divByZeroError
        Right(42).flatMap(oneDivX) shouldBe Right(1 / 42)
        Right(0).flatMap(oneDivX) shouldBe divByZeroError
      }
    }
    "providing fallback logic with orElse" should {
      "return self without evaluating provided fallback expression" in {
        Right(0).orElse(Right(42)) shouldBe Right(0)
        Right(0).orElse(Left("Misterious error")) shouldBe Right(0)
        Right(0).orElse(throw new RuntimeException) shouldBe Right(0)
      }
    }
    "trying to combine it with other either value" when {
      val intOrError = Right(42)
      "combining with right either" should {
        "return right either containing combined value" in {
          intOrError.map2(Right(1))(_ + _) shouldBe Right(43)
          intOrError.map2(Right(" banana"))(_ + _) shouldBe Right("42 banana")
        }
      }
      "combining with left either" should {
        "return error value from left either operand without evaluating provided function" in {
          intOrError.map2(Left("failed"))(_ + _) shouldBe Left("failed")
          intOrError.map2(Left("failed"))((_, _) => throw new RuntimeException) shouldBe Left("failed")
        }
      }
    }
  }

  "left either" when {
    val err: Either[String, Int] = Left("error")
    "trying to map its value" should {
      "return self without evaluating provided function" in {
        err.map(_ => throw new RuntimeException) shouldBe err
      }
    }
    "applying another operation to its value with flatMap" should {
      "return self without evaluating provided function" in {
        err.flatMap(_ => throw new RuntimeException) shouldBe err
      }
    }
    "providing fallback logic with orElse" should {
      "return provided fallback value (left or right either)" in {
        err.orElse(Right(0)) shouldBe Right(0)
        err.orElse(Left("another error")) shouldBe Left("another error")
      }
    }
    "trying to combine it with other either value" should {
      "return self" in {
        err.map2(Right(42))((a, b) => a + b) shouldBe err
      }
    }
  }

}
