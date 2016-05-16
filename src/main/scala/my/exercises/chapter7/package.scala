package my.exercises

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

package object chapter7 {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isCancelled: Boolean = false
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isDone: Boolean = true
  }

  object Par {
    def unit[A](a: A): Par[A] = _ => UnitFuture(a)
    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        override def call(): A = a(es).get
      })
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      es => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1) {
      Par.unit(ints.headOption.getOrElse(0))
    } else {
      val (l, r) = ints.splitAt(ints.size / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

}



