package fpis.chap2

object MyModule {
  def abs(n: Int) =
    if (n < 0) -n else n

  def formatAbs(x: Int) =
    s"The absolute value of $x is ${abs(x)}"

}
