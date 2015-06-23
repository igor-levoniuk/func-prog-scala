package my.exercises

package object chapter5 {

  trait Stream[+A] {

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

    def toList: List[A] = this match {
      case Empty => List.empty[A]
      case Cons(a, as) => a() :: as().toList
    }

    def take(n: Int): Stream[A] = this match {
      case Empty => Empty
      case _  if n <= 0 => Empty
      case Cons(a, as) => Stream.cons(a(), as().take(n - 1))
    }

    def drop(n: Int): Stream[A] = this match {
      case Empty => Empty
      case stream if n <= 0 => stream
      case Cons(_, as) => as().drop(n - 1)
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(a, as) if p(a()) => Stream.cons(a(), as().takeWhile(p))
      case _ => Empty
    }
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
  }

}
