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

  def map[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] = r ⇒ {
    val (a, r2) = s(r)
    (f(a), r2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = r ⇒ {
    val (a, r1) = ra(r)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  def mySequence[A](fs: List[Rand[A]]): Rand[List[A]] = r ⇒ {
    fs.reverse.foldLeft((Nil: List[A], r)) {
      case ((l, rx), ra) ⇒ {
        val (a, rxn) = ra(rx)
        (a :: l, rxn)
      }
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](unit(Nil))((ra, racc) ⇒ map2(ra, racc)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = r ⇒ {
    val (a, r1) = f(r)
    g(a)(r1)
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
    (1 to count).foldLeft((Nil: List[Int], r))((p, _) ⇒ {
      val (i, nr) = p._2.nextInt
      (i :: p._1, nr)
    })
  }

  def positiveMax(n: Int): Rand[Int] = map(positiveInt)(_ / (Int.MaxValue / n))

  val double_1: Rand[Double] = map(positiveInt)(_ / (Int.MaxValue.toDouble + 1))

  val intDouble_1: Rand[(Int, Double)] = map2(positiveInt, double)((_, _))

  val doubleInt_1: Rand[(Double, Int)] = map2(double, positiveInt)((_, _))

  def ints_1(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def positiveInt_1(r: RNG): (Int, RNG) =
    flatMap(int)(a ⇒ if (a == Int.MinValue) positiveInt_1 else unit(a.abs))(r)

  def map_1[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] = flatMap(s)(f andThen unit)

  def map2_1[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a ⇒ map(rb)(b ⇒ f(a, b)))
}

case class State[S, +A](run: S ⇒ (A, S)) {
  import State._

  def flatMap[B](g: A => State[S, B]): State[S, B] = State(s ⇒ {
    val (a, s1) = run(s)
    g(a).run(s1)
  })

  def map[B](f: A ⇒ B): State[S, B] = flatMap(f andThen unit)

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a ← this
      b ← sb 
    } yield f(a, b)
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s ⇒ (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight[State[S, List[A]]](unit(Nil))((ra, racc) ⇒ ra.map2(racc)(_ :: _))

}
