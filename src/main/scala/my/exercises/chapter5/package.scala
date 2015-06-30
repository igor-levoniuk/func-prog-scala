package my.exercises

package object chapter5 {

  trait Stream[+A] {
    import Stream._

    def head: A = this match {
      case Empty => throw new NoSuchElementException("head of empty Stream")
      case Cons(a, _) => a()
    }

    def tail: Stream[A] = this match {
      case Empty => throw new NoSuchElementException("tail of empty Stream")
      case Cons(_, as) => as()
    }

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(a, _) => Some(a())
    }

    def headOptionFR: Option[A] = foldRight(Option.empty[A])((a, _) => Some(a))

    def toList: List[A] = this match {
      case Empty => List.empty[A]
      case Cons(a, as) => a() :: as().toList
    }

    def take(n: Int): Stream[A] = this match {
      case Empty => Empty
      case _  if n <= 0 => Empty
      case Cons(a, as) => cons(a(), as().take(n - 1))
    }

    def drop(n: Int): Stream[A] = this match {
      case Empty => Empty
      case stream if n <= 0 => stream
      case Cons(_, as) => as().drop(n - 1)
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(a, as) if p(a()) => cons(a(), as().takeWhile(p))
      case _ => Empty
    }

    def takeWhileFR(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else Empty)

    def foldRight[B](z: B)(f: (=> A, => B) => B): B = this match {
      case Cons(a, as) => f(a(), as().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

    def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

    def append[AA >: A](as: => Stream[AA]): Stream[AA] = foldRight(as)((a, b) => cons(a, b))

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](private val h: () => A, private val t: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def fibs: Stream[Int] = {
      def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a + b))
      go(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case None => Empty
        case Some((a, s)) => cons(a, unfold(s)(f))
      }
  }

}
