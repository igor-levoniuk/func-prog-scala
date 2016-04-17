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

  type Rand[+T] = Rng => (T, Rng)

  val int: Rand[Int] = rng => rng.nextInt

  val double: Rand[Double] = Rng.double

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](rand: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, nextRng) = rand(rng)
      (f(a), nextRng)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, aRng) = ra(rng)
      val (b, bRng) = rb(aRng)
      (f(a, b), bRng)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)(Tuple2.apply)

  val intDoubleUsingMap2: Rand[(Int, Double)] = both(int, double)

  val doubleIntUsingMap2: Rand[(Double, Int)] = both(double, int)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng => rs.foldRight((List.empty[A], rng)) {
      case (ra, (as, currRng)) =>
        val (a, nextRng) = ra(currRng)
        (a :: as, nextRng)
    }

  def sequence2[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng => rs.foldRight((List.empty[A], rng)) {
      case (ra, (as, currRng)) => map2(ra, unit(as))(_ :: _)(currRng)
    }

  def sequence3[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, nextRng) = ra(rng)
      f(a)(nextRng)
    }

  def nonNegativeIntLessThen(n: Int): Rand[Int] =
    flatMap(Rng.nonNegativeInt) {
      i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod)
        else nonNegativeIntLessThen(n)
    }

  def mapUsingFlatMap[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra)(a => unit(f(a)))

  def map2UsingFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))


  case class State[S, +A](run: S => (S, A)) {

    def map[B](f: A => B): State[S, B] =
      flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(
      s => {
        val (s1, a) = run(s)
        f(a).run(s1)
      }
    )
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (s, a))

    def sequence[S, B](xs: List[State[S, B]]): State[S, List[B]] =
      xs.foldRight(unit[S, List[B]](List.empty))((s1, sb) => s1.map2(sb)(_ :: _))
  }

}
