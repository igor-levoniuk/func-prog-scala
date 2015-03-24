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

    def foldLeft[A, B](xs: List[A], z: B)(f: (A, => B) => B): B =
      xs match {
        case Nil => z
        case Cons(y, ys) => foldLeft(ys, f(y, z))(f)
      }

    def sum(xs: List[Int]): Int =
      foldRight(xs, 0)(_ + _)

    def sumFoldLeft(xs: List[Int]): Int =
      foldLeft(xs, 0)(_ + _)

    def product(xs: List[Double]): Double =
      foldRight(xs, 1.0)(_ * _)

    def productFoldLeft(xs: List[Double]): Double =
      foldLeft(xs, 1.0)(_ * _)

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

    def length[A](xs: List[A]): Int =
     foldRight(xs, 0)((_,b) => b + 1)

    def lengthFoldLeft[A](xs: List[A]): Int =
      foldLeft(xs, 0)((_, b) => b + 1)

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

    def reverse[A](xs: List[A]): List[A] =
      foldLeft(xs, Nil: List[A])(Cons(_, _))

    def foldRightUsingFoldLeft[A, B](xs: List[A], z: B)(f: (A, => B) => B): B =
      foldLeft(foldLeft(xs, Nil: List[A])(Cons(_, _)), z)(f)

    def append[A](xs: List[A], x: A): List[A] =
      foldRightUsingFoldLeft(xs, Cons(x, Nil))(Cons(_, _))

    def flatten[A](xs: List[List[A]]): List[A] =
      xs match {
        case Nil => Nil
        case Cons(ys, ls) => foldRightUsingFoldLeft(ys, flatten(ls))(Cons(_, _))
      }

    def plusOne(xs: List[Int]): List[Int] =
      foldRightUsingFoldLeft(xs, Nil: List[Int]) {
        (x, l) => Cons(x + 1, l)
      }

    def asStrings[A](xs: List[A]): List[String] =
      foldRightUsingFoldLeft(xs, Nil: List[String]) {
        (x, l) => Cons(x.toString, l)
      }

    def map[A, B](xs: List[A])(f: A => B): List[B] =
      xs match {
        case Nil => Nil
        case Cons(y, ys) => Cons(f(y), map(ys)(f))
      }

    def tailRecursiveMap[A, B](xs: List[A])(f: A => B): List[B] =
      foldRightUsingFoldLeft(xs, Nil: List[B]) {
        (x, l) => Cons(f(x), l)
      }

    def filter[A](xs: List[A])(p: A => Boolean): List[A] =
      xs match {
        case Nil => Nil
        case Cons(y, ys) => if (p(y)) Cons(y, filter(ys)(p)) else filter(ys)(p)
      }

    def tailRecursiveFilter[A](xs: List[A])(p: A => Boolean): List[A] =
      foldRightUsingFoldLeft(xs, Nil: List[A]) {
        (x, l) => if (p(x)) Cons(x, l) else l
      }

    def filterUsingFlatMap[A](xs: List[A])(p: A => Boolean): List[A] =
      flatMap(xs)(x => if (p(x)) List(x) else Nil)

    def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] =
      flatten(map(xs)(f))

    def tailRecursiveFlatMap[A, B](xs: List[A])(f: A => List[B]): List[B] =
      foldRightUsingFoldLeft(xs, Nil: List[B]) {
        (x, l) => foldRightUsingFoldLeft(f(x), l)(Cons(_, _))
      }

    def sumLists(xs: List[Int], ys: List[Int]): List[Int] =
      (xs, ys) match {
        case (Nil, Nil) => Nil
        case (Cons(x1, xs1), Nil)  => Cons(x1, sumLists(xs1, Nil))
        case (Nil, Cons(y1, ys1)) => Cons(y1, sumLists(ys1, Nil))
        case (Cons(x1, xs1), Cons(y1, ys1))  => Cons(x1 + y1, sumLists(xs1, ys1))
      }

    def zipWith[A](xs: List[A], ys: List[A])(f: (A, A) => A): List[A] =
      (xs, ys) match {
        case (Nil, Nil) => Nil
        case (Cons(x1, xs1), Nil)  => Cons(x1, zipWith(xs1, Nil)(f))
        case (Nil, Cons(y1, ys1)) => Cons(y1, zipWith(ys1, Nil)(f))
        case (Cons(x1, xs1), Cons(y1, ys1))  => Cons(f(x1, y1), zipWith(xs1, ys1)(f))
      }

    def hasSubsequence[A](xs: List[A], sub: List[A]): Boolean = {
      def sameElements(ys: List[A], zs: List[A]): Boolean =
        (ys, zs) match {
          case (_, Nil) => true
          case (Cons(y, ys1), Cons(z, zs1)) if y == z => sameElements(ys1, zs1)
          case _ => false
        }

      xs match {
        case Nil => sub == Nil
        case l @ Cons(_, ys) => sameElements(l, sub) || hasSubsequence(ys, sub)
      }
    }

  }


  sealed trait Tree[+A]
  case class Leaf[+A](value: A) extends Tree[A]
  case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    def size(tree: Tree[_]): Int =
      tree match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
      }

    def maximum(tree: Tree[Int]): Int =
      tree match {
        case Leaf(x) => x
        case Branch(l, r) => maximum(l) max maximum(r)
      }

    def depth(tree: Tree[_]): Int =
      tree match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + (depth(l) max depth(r))
      }

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      tree match {
        case Leaf(x) => Leaf(f(x))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      }

    def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B =
      tree match {
        case Leaf(x) => f(x)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
      }

    def sizeUsingFold(tree: Tree[_]): Int = fold(tree)(_ => 1)(_ + _ + 1)

    def maximumUsingFold(tree: Tree[Int]): Int = fold(tree)(identity)(_ max _)

    def depthUsingFold(tree: Tree[_]): Int = fold(tree)(_ => 1)((l, r) => 1 + (l max r))

    def mapUsingFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      fold[A, Tree[B]](tree)(x => Leaf(f(x)))((l, r) => Branch(l, r))
  }
}

