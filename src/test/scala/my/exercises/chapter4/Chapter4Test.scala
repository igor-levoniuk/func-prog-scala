package my.exercises.chapter4

import org.scalatest.{Inside, ShouldMatchers, WordSpec}

class Chapter4Test extends WordSpec with ShouldMatchers with Inside {

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

  "Option companion object" should {
    "have method Try, such that" when {
      "provided function ends normally" should {
        "return option containing function result" in {
          Option.Try(42) shouldBe Some(42)
          Option.Try("foo" + "bar") shouldBe Some("foobar")
        }
      }
      "provided function throws an exception" should {
        "return empty option" in {
          Option.Try(throw new RuntimeException) shouldBe None
          Option.Try(42 / 0) shouldBe None
          Option.Try(List.empty.head) shouldBe None
        }
      }
    }
    "calculate mean of a sequence of numbers should be calculated" in {
      Option.mean(Seq.empty) shouldBe None
      Option.mean(Seq(1, 1, 1)) shouldBe Some(1.0)
      Option.mean(Seq(1, 2, 3)) shouldBe Some(2.0)
      Option.mean(Seq(100, 10, 52)) shouldBe Some(54.0)
    }
    "calculate variance of a sequence of numbers should be calculated" in {
      Option.variance(Seq.empty) shouldBe None
      Option.variance(Seq(1, 1, 1)) shouldBe Some(0)
      Option.variance(Seq(1, 2, 3, 2, 1)) shouldBe Some(0.56)
      Option.variance(Seq(5, 6, 9, 7)) shouldBe Some(2.1875)
    }
    "combine two Options with map2 in a way that" when {
      "at least one option is empty" should {
        "return empty option" in {
          Option.map2(None, None)((_, _) => throw new RuntimeException) shouldBe None
          Option.map2(Some(42), None)((_, _) => throw new RuntimeException) shouldBe None
          Option.map2(None, Some(42))((_, _) => throw new RuntimeException) shouldBe None
        }
      }
      "both options are non-empty" should {
        "return non-empty option containing result of specified function" in {
          Option.map2(Some(42), Some(1))(_ + _) shouldBe Some(43)
          Option.map2(Some("foo"), Some("bar"))(_ + _) shouldBe Some("foobar")
          Option.map2(Some("foo"), Some(42))(_ + _) shouldBe Some("foo42")
        }
      }
    }
    "process sequence of option in a way that" when {
      "called on a list containing empty option(s)" should {
        "return empty option" in {
          Option.sequence(List(None, None, None)) shouldBe None
          Option.sequence(List(Some(42), Some("foo"), None)) shouldBe None
        }
      }
      "called on a list containing only non-empty options" should {
        "return List with options flattened (extracted options values)" in {
          Option.sequence(List.empty) shouldBe Some(List.empty)
          Option.sequence(List(Some(42), Some("foo"), Some(true))) shouldBe Some(List(42, "foo", true))
        }
      }
    }
    "traverse over a list in a way that" when {
      "whole list can be converted to non-empty options with provided function" should {
        "return non-empty option on a list of flattened (extracted) option values" in {
          Option.traverse(List(1, 2, 3, 4, 5))(x => Option.Try(x.toString)) shouldBe Some(List("1", "2", "3", "4", "5"))
          Option.traverse(List("1", "2", "3", "4", "5"))(x => Option.Try(x.toInt)) shouldBe Some(List(1, 2, 3, 4, 5))
        }
      }
      "only some elements of the list can be converted to non-empty options" should {
        "return empty option" in {
          Option.traverse(List(1, 2, 3, 4, 5))(x => Option.Try(x / (x - 3))) shouldBe None
          Option.traverse(List("1", "2", "3", "banana", "5"))(x => Option.Try(x.toInt)) shouldBe None
        }
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

  "Either companion object" should {
    "have method Try, such that" when {
      "provided function ends normally" should {
        "return Right either containing function result" in {
          Either.Try(42) shouldBe Right(42)
          Either.Try("foo" + "bar") shouldBe Right("foobar")
        }
      }
      "provided function throws an exception" should {
        "return left either containing the exception thrown" in {
          inside(Either.Try(throw new RuntimeException("failed"))) {
            case Left(e: Exception) =>
              e.getClass shouldBe classOf[RuntimeException]
              e.getMessage shouldBe "failed"
            case x => fail(s"$x was not Left[Exception]")
          }
          inside(Either.Try(42 / 0)) {
            case Left(e: Exception) =>
              e.getClass shouldBe classOf[ArithmeticException]
              e.getMessage shouldBe "/ by zero"
            case x => fail(s"$x was not Left[Exception]")
          }
          inside(Either.Try(List.empty.head)) {
            case Left(e: Exception) =>
              e.getClass shouldBe classOf[NoSuchElementException]
              e.getMessage shouldBe "head of empty list"
            case x => fail(s"$x was not Left[Exception]")
          }
        }
      }
    }
    "process sequence of Either values in a way that" when {
      "called on a list containing Left values" should {
        "return empty first Left value" in {
          val failed = Left("failed")
          Either.sequence(List(Right(42), Right(0), failed)) shouldBe failed
          Either.sequence(List(Right(42), Right("foo"), failed)) shouldBe failed
        }
      }
      "called on a list containing only right values" should {
        "return List of extracted Either values" in {
          Either.sequence(List.empty) shouldBe Right(List.empty)
          Either.sequence(List(Right(42), Right("foo"), Right(true))) shouldBe Right(List(42, "foo", true))
        }
      }
    }
    "traverse over a list in a way that" when {
      "whole list can be converted to right either with provided function" should {
        "return right either containing a list of converted values" in {
          Either.traverse(List(1, 2, 3, 4, 5))(x => Either.Try(x.toString)) shouldBe Right(List("1", "2", "3", "4", "5"))
          Either.traverse(List("1", "2", "3", "4", "5"))(x => Either.Try(x.toInt)) shouldBe Right(List(1, 2, 3, 4, 5))
        }
      }
      "only some elements of the list can be converted" should {
        "return left either containing error" in {
          inside(Either.traverse(List(1, 2, 3, 4, 5))(x => Either.Try(x / (x - 3)))) {
            case Left(e: Exception) =>
              e.getClass shouldBe classOf[ArithmeticException]
              e.getMessage shouldBe "/ by zero"
            case x => fail(s"$x was not Left[Exception]")
          }
          inside(Either.traverse(List("1", "2", "3", "banana", "5"))(x => Either.Try(x.toInt))) {
            case Left(e: Exception) =>
              e.getClass shouldBe classOf[NumberFormatException]
              e.getMessage shouldBe "For input string: \"banana\""
            case x => fail(s"$x was not Left[Exception]")
          }
        }
      }
    }
  }
}
