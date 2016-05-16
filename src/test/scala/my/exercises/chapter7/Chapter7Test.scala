package my.exercises.chapter7

import java.util.concurrent.{Executors, ThreadPoolExecutor}

import org.scalatest.{ShouldMatchers, WordSpec}

class Chapter7Test extends WordSpec with ShouldMatchers {

  private val NumberOfThreads = 10
  val executorService = Executors.newFixedThreadPool(NumberOfThreads)

  "sum method" should {
    "calculate sum of empty sequence" in {
      Par.run(executorService)(sum(IndexedSeq.empty[Int])).get shouldBe 0
    }
    "calculate sum of single-element sequence" in {
      Par.run(executorService)(sum(IndexedSeq(0))).get shouldBe 0
      Par.run(executorService)(sum(IndexedSeq(1))).get shouldBe 1
      Par.run(executorService)(sum(IndexedSeq(-1))).get shouldBe -1
      Par.run(executorService)(sum(IndexedSeq(100))).get shouldBe 100
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
        seq => seq.sum shouldEqual Par.run(executorService)(sum(seq)).get
      }
    }
  }
}
