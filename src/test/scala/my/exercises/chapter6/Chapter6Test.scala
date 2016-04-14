package my.exercises.chapter6

import org.scalatest.{ShouldMatchers, WordSpec}

class Chapter6Test extends WordSpec with ShouldMatchers {

  val rng = SimpleRng(42)

  val directRngExtractor: Rand[Int] = _.nextInt

  def take[E](n: Int, rng: Rng, extractor: Rng => (E, Rng) = directRngExtractor): List[E] =
    if (n <= 0) List.empty[E]
    else {
      val (value, nextRng) = extractor(rng)
      value :: take(n - 1, nextRng, extractor)
    }

  case class MockRng(value: Int) extends Rng {
    override def nextInt: (Int, Rng) = (value, this)
  }

  "Random number generator" should {
    "be be consistent" in {
      val rng42Value = rng.nextInt._1
      for (_ <- 1 to 10) {
         rng.nextInt._1 shouldBe rng42Value
      }
    }
    "be able to generate Int values" in {
      val ints = take(10, rng)
      ints.foreach {
        x => ints.count(_ == x) shouldBe 1
      }
    }
    "be able to generate non-negative Int value" in {
      take(100, rng, Rng.nonNegativeInt).foreach {
        _ should be >= 0
      }
      Rng.nonNegativeInt(MockRng(Int.MinValue))._1 should be >= 0
    }
    "be able to generate Double value" in {
      Rng.double(MockRng(Int.MinValue))._1 should (be >= 0.0 and be < 1.0)
      Rng.double(MockRng(Int.MaxValue))._1 should (be >= 0.0 and be < 1.0)
      val doubles = take(100, rng, Rng.double)
      doubles.foreach { x =>
        x should (be >= 0.0 and be < 1.0)
        doubles.count(_ == x) shouldBe 1
      }

      Rng.doubleUsingMap(MockRng(Int.MinValue))._1 should (be >= 0.0 and be < 1.0)
      Rng.doubleUsingMap(MockRng(Int.MaxValue))._1 should (be >= 0.0 and be < 1.0)
      val doubles2 = take(100, rng, Rng.doubleUsingMap)
      doubles2.foreach { x =>
        x should (be >= 0.0 and be < 1.0)
        doubles2.count(_ == x) shouldBe 1
      }
    }
    "be able to generate (Int, Double) and (Double, Int) tupled of values" in {
      val intDoubles = take(100, rng, Rng.intDouble)
      val (ints1, doubles1) = intDoubles.unzip
      intDoubles.foreach {
        case (intVal, doubleVal) =>
          ints1.count(_ == intVal) shouldBe 1
          doubles1.count(_ == doubleVal) shouldBe 1
          doubleVal should (be >= 0.0 and be < 1.0)
      }

      val intDoubles2 = take(100, rng, intDoubleUsingMap2)
      val (ints21, doubles21) = intDoubles2.unzip
      intDoubles2.foreach {
        case (intVal, doubleVal) =>
          ints21.count(_ == intVal) shouldBe 1
          doubles21.count(_ == doubleVal) shouldBe 1
          doubleVal should (be >= 0.0 and be < 1.0)
      }

      val doubleInts = take(100, rng, Rng.doubleInt)
      val (doubles2, ints2) = doubleInts.unzip
      doubleInts.foreach {
        case (doubleVal, intVal) =>
          ints2.count(_ == intVal) shouldBe 1
          doubles2.count(_ == doubleVal) shouldBe 1
          doubleVal should (be >= 0.0 and be < 1.0)
      }

      val doubleInts2 = take(100, rng, doubleIntUsingMap2)
      val (doubles22, ints22) = doubleInts2.unzip
      doubleInts2.foreach {
        case (doubleVal, intVal) =>
          ints22.count(_ == intVal) shouldBe 1
          doubles22.count(_ == doubleVal) shouldBe 1
          doubleVal should (be >= 0.0 and be < 1.0)
      }
    }
    "be able to generate triplet of Double values" in {
      val doubleTriplets = take(100, rng, Rng.double3)
      doubleTriplets.foreach {
        case (d1, d2, d3) =>
          d1 should not be d2
          d1 should not be d3
          d2 should not be d3

          d1 should (be >= 0.0 and be < 1.0)
          d2 should (be >= 0.0 and be < 1.0)
          d3 should (be >= 0.0 and be < 1.0)

      }
    }
    "be able to generate list of Int values" in {
      Rng.ints(-1)(rng)._1 shouldBe empty
      Rng.ints(0)(rng)._1 shouldBe empty
      val (ints100, _) = Rng.ints(100)(rng)
      ints100 should have size 100
      ints100.distinct should have size 100
    }
    "be able to convert sequence of Rngs to Rng of a sequence" in {
      sequence(List(unit(1), unit(2), unit(3)))(rng)._1 shouldBe List(1, 2, 3)
      sequence2(List(unit(1), unit(2), unit(3)))(rng)._1 shouldBe List(1, 2, 3)
    }
  }
}
