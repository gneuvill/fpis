package fpis.chap6.state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

  type Rand[+A] = RNG ⇒ (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = r ⇒ (a, r)

  def map[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] =
    r ⇒ {
      val (a, r2) = s(r)
      (f(a), r2)
    }

  def positiveInt(r: RNG): (Int, RNG) = {
    val (i, r2) = r.nextInt
    if (i == Int.MinValue) positiveInt(r2) else (i.abs, r2)
  }

  def double(r: RNG): (Double, RNG) = {
    val (i, r2) = positiveInt(r)
    (i / (Int.MaxValue.toDouble + 1), r2)
  }

  def intDouble(r: RNG): ((Int, Double), RNG) = {
    val (i, r2) = positiveInt(r)
    val (d, r3) = double(r2)
    ((i, d), r3)
  }

  def doubleInt(r: RNG): ((Double, Int), RNG) = {
    val ((i, d), r2) = intDouble(r)
    ((d, i), r2)
  }

  def double3(r: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r2) = double(r)
    val (d2, r3) = double(r2)
    val (d3, r4) = double(r3)
    ((d1, d2, d3), r4)
  }

  def ints(count: Int)(r: RNG): (List[Int], RNG) = {
    (1 to count).toList.foldLeft((Nil: List[Int], r))((p, _) ⇒ {
      val (i, nr) = p._2.nextInt
      (i :: p._1, nr)
    })
  }

  def positiveMax(n: Int): Rand[Int] = map(positiveInt)(_ / (Int.MaxValue / n))

  val double_2: Rand[Double] = map(positiveInt)(_ / (Int.MaxValue.toDouble + 1))

}

