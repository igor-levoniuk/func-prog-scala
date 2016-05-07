package my.exercises.chapter7

import org.scalatest.{ShouldMatchers, WordSpec}

class Chapter7Test extends WordSpec with ShouldMatchers {

  "sum method" should {
    "calculate sum of empty sequence" in {
      Par.get(sum(IndexedSeq.empty[Int])) shouldBe 0
    }
    "calculate sum of single-element sequence" in {
      Par.get(sum(IndexedSeq(0))) shouldBe 0
      Par.get(sum(IndexedSeq(1))) shouldBe 1
      Par.get(sum(IndexedSeq(-1))) shouldBe -1
      Par.get(sum(IndexedSeq(100))) shouldBe 100
    }
    "calculate sum of arbitrary sequence" in {
      val testSequences = Seq(
        IndexedSeq(1, 2, 3, 4, 5),
        IndexedSeq.empty[Int],
        IndexedSeq(42),
        IndexedSeq(-1, 1000, 10, -14, 42, 100),
        IndexedSeq(90, -1000, 12, 70)
      )

      testSequences.foreach {
        seq => seq.sum shouldEqual Par.get(sum(seq))
      }
    }
  }
}
