package my.exercises

package object chapter3 {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def foldRight[A, B](xs: List[A], z: B)(f: (A, => B) => B): B =
      xs match {
        case Nil => z
        case Cons(y, ys) => f(y, foldRight(ys, z)(f))
      }

    def sum(xs: List[Int]): Int =
      foldRight(xs, 0)(_ + _)

    def product(xs: List[Double]): Double =
      foldRight(xs, 1.0)(_ * _)

    def smartProduct(xs: List[Double]): Double = {
      def smartReduce(a: Double, b: => Double): Double =
        if (a == 0) 0 else a * b

      foldRight(xs, 1.0)(smartReduce)
    }

    /**
     * Need method this to test smartProduct.
     * Note: this is why mutation is hard - swap return type to be (Int, Double)
     * and counting will not work (even though it's not hard ot fix).
     */
    def iterationCountingProduct(xs: List[Double]): (Double, Int) = {
      var n = 0
      def smartReduce(a: Double, b: => Double): Double = {
        n = n + 1
        if (a == 0) 0 else a * b
      }

      (foldRight(xs, 1.0)(smartReduce), n)
    }

    def apply[A](xs: A*): List[A] =
      if (xs.isEmpty) Nil
      else Cons(xs.head, apply(xs.tail: _*))

    def tail[A](xs: List[A]): List[A] =
      xs match {
        case Nil => Nil
        case Cons(y, ys) => ys
      }

    def tailOption[A](xs: List[A]): Option[List[A]] =
      xs match {
        case Nil => None
        case Cons(y, ys) => Some(ys)
      }
    
    def setHead[A](h: A, xs: List[A]): List[A] =
      xs match {
        case Nil => Cons(h, Nil)
        case Cons(y, ys) => Cons(h, ys)
      }

    def drop[A](xs: List[A], n: Int): List[A] =
      xs match {
        case Nil => Nil
        case Cons(_, ys) if n > 0 => drop(ys, n - 1)
        case ys => ys
      }

    def dropWhile[A](xs: List[A])(p: A => Boolean): List[A] =
      xs match {
        case Nil => Nil
        case Cons(y, ys) if p(y) => dropWhile(ys)(p)
        case ys => ys
      }

    def init[A](xs: List[A]): List[A] =
      xs match {
        case Nil | Cons(_, Nil) => Nil
        case Cons(y, ys) => Cons(y, init(ys))
      }
  }

}

