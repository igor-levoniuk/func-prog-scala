package my.exercises

package object chapter7 {

  class Par[A](private val value: A)

  object Par {
    def unit[A](a: => A): Par[A] = new Par(a)
    def get[A](par: Par[A]): A = par.value
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = unit(f(get(a), get(b)))
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) {
      Par.unit(ints.headOption.getOrElse(0))
    } else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

}



