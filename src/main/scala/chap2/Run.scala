package fpis.chap2

object Run extends App {

  import MyModule._
  println("#### 2.2")
  println(formatAbs(-42))

  println("#### 2.5")
  println(formatResult("abs", -42, abs))
  println("Factorial")
  println(formatFactorial(4))
  println(formatResult("factorial", 4, factorial))
  println("Fibonacci")
  println(fib(7))
  println(formatResult("fibonacci", 7, fib))

  import PolymorphicFunctions._
  println("#### 2.6")
  println(isSorted[Int](Array(11, 10, 30, 31), _ >= _))
  println(formatResult("add10", 20, add10))
}

