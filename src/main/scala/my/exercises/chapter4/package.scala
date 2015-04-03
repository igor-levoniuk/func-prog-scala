package my.exercises


package object chapter4 {

  sealed trait Option[+A] {
    def get: A
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def filter(p: A => Boolean): Option[A]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](default: => Option[B]): Option[B]
  }

  case object None extends Option[Nothing] {
    override def get: Nothing = throw new NoSuchElementException
    override def map[B](f: (Nothing) => B): Option[B] = None
    override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None
    override def filter(p: (Nothing) => Boolean): Option[Nothing] = None
    override def getOrElse[B >: Nothing](default: => B): B = default
    override def orElse[B >: Nothing](default: => Option[B]): Option[B] = default
  }

  case class Some[+A](get: A) extends Option[A] {
    override def map[B](f: A => B): Option[B] = Some(f(get))
    override def flatMap[B](f: A => Option[B]): Option[B] = f(get)
    override def filter(p: A => Boolean): Option[A] = if (p(get)) this else None
    override def getOrElse[B >: A](default: => B): B = get
    override def orElse[B >: A](default: => Option[B]): Option[B] = this
  }

  def Try[A](expression: => A): Option[A] =
    try {
      Some(expression)
    } catch {
      case e: Exception => None
    }

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = for {
    meanValue <- mean(xs)
    varianceValue <- mean(xs.map(x => math.pow(x - meanValue, 2)))
  } yield varianceValue

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for (v1 <- a; v2 <- b) yield f(v1, v2)

  def sequence[A](xs: List[Option[A]]): Option[List[A]] =
    traverse(xs)(identity)

  def traverse[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] =
    xs.foldRight[Option[List[B]]](Some(Nil)) {
      (a, acc) => f(a) match {
        case None => None
        case Some(b) => acc.map(b :: _)
      }
    }


  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
    def orElse[EE >: E, B >: A](default: => Either[EE, B]): Either[EE, B]
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
  }

  case class Left[+E](value: E) extends Either[E, Nothing] {
    override def map[B](f: Nothing => B): Either[E, B] = this
    override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this
    override def orElse[EE >: E, B](default: => Either[EE, B]): Either[EE, B] = default
    override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
  }

  case class Right[+A](value: A) extends Either[Nothing, A] {
    override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))
    override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]): Either[EE, B] = f(value)
    override def orElse[EE >: Nothing, B >: A](default: => Either[EE, B]): Either[EE, B] = this
    override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b.map(f(value, _))
  }

}
