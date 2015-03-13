package my.exercises

package object chapter3 {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(xs: List[Int]): Int =
      xs match {
        case Nil => 0
        case Cons(y, ys) => y + sum(ys)
      }

    def product(xs: List[Double]): Double =
      xs match {
        case Nil => 1
        case Cons(y, ys) => y * product(ys)
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

