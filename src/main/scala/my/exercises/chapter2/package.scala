package my.exercises

package object chapter2 {

  def fib(n: Int): Int = {
    def go(i: Int, curr: Int, next: Int): Int =
      if (i >= n) curr else go(i + 1, next, curr + next)

    go(0, 0, 1)
  }

  def isSorted[A](as: Array[A], ordering: (A,A) => Boolean): Boolean = {
    def go(i: Int, res: Boolean): Boolean =
      if (res && i < as.length)
        go(i + 1, ordering(as(i - 1), as(i)))
      else
        res

    go(1, res = true)
  }


}
