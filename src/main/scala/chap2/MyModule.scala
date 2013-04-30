package fpis.chap2

import scala.annotation._

object MyModule {
  def abs(n: Int) =
    if (n < 0) -n else n

  def formatAbs(x: Int) =
    s"The absolute value of $x is ${abs(x)}"

  def factorial(n: Int): Int = {
    @tailrec
    def go(x: Int, acc: Int): Int =
      if (x <= 0) acc
      else go(x - 1, acc * x)
    go(n, 1)
  }

  def fib(n: Int): Int = {
    def go(x: Int, prev: Int, cur: Int): Int =
      if (x >= n) prev
      else go(x + 1,  cur, prev + cur)
    go(1, 0, 1)
  }

}
