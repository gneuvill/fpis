package fpis.chap2

import scala.annotation._

object MyModule {
  def abs(n: Int) =
    if (n < 0) - n else n

  def formatAbs(x: Int) =
    s"The absolute value of $x is ${abs(x)}"

  def factorial(n: Int): Int = {
    @tailrec
    def go(x: Int, acc: Int): Int =
      if (x <= 0) acc
      else go(x - 1, acc * x)
    go(n, 1)
  }

  def formatFactorial(x: Int) =
    s"The factorial of $x is ${factorial(x)}"

  def fib(n: Int): Int = {
    @tailrec
    def go(x: Int, prev: Int, cur: Int): Int =
      if (x >= n) prev
      else go(x + 1,  cur, prev + cur)
    go(1, 0, 1)
  }

  /**
    * Let's generalize to a single presentation function
    */
  def formatResult(name: String, n: Int, f: Int ⇒ Int) =
    s"The result of $name applied to $n is ${f(n)}"

}

object PolymorphicFunctions {

  def isSorted[A](as: Array[A], gt: (A,A) ⇒ Boolean): Boolean = {
    @tailrec
    def go(n: Int, bool: Boolean): Boolean =
      if ((n < as.length - 1) && bool)
        go(n + 1, bool && gt(as(n + 1), as(n)))
      else bool
    go(0, true)
  }

  def partial1[A,B,C](a: A, f: (A,B) ⇒ C): B ⇒ C = b ⇒ f(a, b)

  // an example of usage of partial1
  def add(n1: Int, n2: Int): Int = n1 + n2
  def add10  = partial1(10, add)

  def curry[A,B,C](f: (A, B) ⇒ C): A ⇒ (B ⇒ C) = a ⇒ b ⇒ f(a, b)

  def uncurry[A,B,C](f: A ⇒ B ⇒ C): (A, B) ⇒ C = (a, b) ⇒ f(a)(b)

  def compose[A,B,C](f: B ⇒ C, g: A ⇒ B): A ⇒ C = a ⇒ f(g(a))
}
