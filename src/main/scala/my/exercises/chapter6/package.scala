package my.exercises

package object chapter6 {

  trait Rng {
    def nextInt: (Int, Rng)
  }

  case class SimpleRng(seed: Long) extends Rng {
    def nextInt: (Int, Rng) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val newRng = SimpleRng(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, newRng)
    }
  }

  object Rng {

    def nonNegativeInt(rng: Rng): (Int, Rng) = {
      val (value, nextRng) = rng.nextInt
      if (value == Int.MinValue) (Int.MaxValue, nextRng)
      else (math.abs(value), nextRng)
    }

    def double(rng: Rng): (Double, Rng) = {
      val (value, nextRng) = nonNegativeInt(rng)
      if (value == Int.MaxValue) (0.0, nextRng)
      else (value.toDouble / Int.MaxValue, nextRng)
    }

    def doubleUsingMap: Rand[Double] =
      map(nonNegativeInt)(x => if (x == Int.MaxValue) 0.0 else x.toDouble / Int.MaxValue)

    def intDouble(rng: Rng): ((Int, Double), Rng) = {
      val (intVal, doubleRng) = rng.nextInt
      val (doubleVal, nextRng) = double(doubleRng)
      ((intVal, doubleVal), nextRng)
    }

    def doubleInt(rng: Rng): ((Double, Int), Rng) = {
      val (intDoubleVal, nextRng) = intDouble(rng)
      (intDoubleVal.swap, nextRng)
    }

    def double3(rng: Rng): ((Double, Double, Double), Rng) = {
      val (doubleVal1, double2Rng) = double(rng)
      val (doubleVal2, double3Rng) = double(double2Rng)
      val (doubleVal3, nextRng) = double(double3Rng)
      ((doubleVal1, doubleVal2, doubleVal3), nextRng)
    }

    def ints(count: Int)(rng: Rng): (List[Int], Rng) =
      if (count <= 0) (List.empty, rng)
      else {
        val (value, nextValuesRng) = rng.nextInt
        val (nextValues, resultRng) = ints(count - 1)(nextValuesRng)
        (value :: nextValues, resultRng)
      }
  }

  type Rand[+A] = Rng => (A, Rng)

  val int: Rand[Int] = rng => rng.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](rand: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, nextRng) = rand(rng)
      (f(a), nextRng)
    }

}