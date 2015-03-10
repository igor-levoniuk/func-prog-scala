package my.exercises.chapter2

object Chapter2Test extends App {

  for (i <- 1 to 10)
    println(s"Fib($i-th)=${fib(i)}")


  isSorted[String](Array("foo", "barr", "bazzz"), _.length < _.length)
  isSorted[String](Array("foo", "barr", "bazzz"), _.length > _.length)
  isSorted[String](Array("foo", "bar", "baz"), _.length == _.length)
  isSorted[Int](Array(1, 2, 5, 100), _ < _)
  isSorted[Int](Array(1, 2, 5, 100), _ > _)
  isSorted[Int](Array(1, 2, 5, 100), _ == _)
  isSorted[Int](Array(1, 1, 1, 1), _ == _)



}
