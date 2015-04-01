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
}
