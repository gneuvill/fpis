package fpis.chap3.datastructures

import scala.annotation._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil ⇒ 0
    case Cons(x, xs) ⇒ x + sum(xs)
  }

  def product(doubles: List[Double]): Double = doubles match {
    case Nil ⇒ 1.0
    case Cons(0.0, Nil) ⇒ 0.0
    case Cons(x, xs) ⇒ x * product(xs)
  }

  // we infer foldRight's implementation by generalizing from sum and product
  def foldRight[A, B](l: List[A], z: B)(f: (A, B) ⇒ B): B = l match {
    case Nil ⇒ z
    case Cons(x, xs) ⇒ f(x, foldRight(xs, z)(f))
  }

  def sumR(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  def productR(doubles: List[Double]): Double = foldRight(doubles, 1.0)(_ * _)

  def lengthR[A](l: List[A]): Int = foldRight(l, 0)((_, acc) ⇒ acc + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil ⇒ z
    case Cons(x, xs) ⇒ foldLeft(xs, f(z, x))(f)
  }

  def sumL(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def productL(doubles: List[Double]): Double = foldLeft(doubles, 1.0)(_ * _)

  def lengthL[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) ⇒ acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) ⇒ Cons(a, b))

  /**
   *  WRONG : see that foldLeft(List(1, 2, 3, 4, 5), Nil: List[Int])((b, a) ⇒ Cons(a, b))
   * isn't equal to
   * foldLeftWrong(List(1, 2, 3, 4, 5), Nil: List[Int])((b, a) ⇒ Cons(a, b))
   */
  def foldLeftWrong[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, z)((a, b) ⇒ f(b, a))

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z)((a, b) ⇒ f(b, a))

  def foldLeft2_1[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) ⇒ b)((a, g) ⇒ b ⇒ g(f(b, a)))(z)

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) ⇒ B): B = foldLeft(reverse(l), z)((b, a) ⇒ f(a, b))

  def foldRight2_1[A, B](l: List[A], z: B)(f: (A, B) ⇒ B): B =
    foldLeft(l, (b: B) ⇒ b)((g, a) ⇒ b ⇒ g(f(a, b)))(z)

  def append[A](l: List[A], r: List[A]): List[A] = foldLeft(reverse(l), r)((b, a) ⇒ Cons(a, b))

  def append2[A](l: List[A], r :List[A]): List[A] = foldRight(l, r)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] = foldLeft(l, Nil: List[A])(append)

  def concat2[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a, acc) ⇒ Cons(a + 1, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((a, acc) ⇒ Cons(a.toString, acc))

  // we infer map's implementation by generalizing from add1 and doubleToString
  def map[A,B](l: List[A])(f: A ⇒ B): List[B] =
    foldRight(l, Nil: List[B])((a, acc) ⇒ Cons(f(a), acc))

  // but for the sake of efficiency, we prefer local mutation
  def map_1[A, B](l: List[A])(f: A ⇒ B): List[B] = {
    val buf = collection.mutable.ListBuffer[B]()
    @tailrec
    def loop(as: List[A]): List[B] = l match {
      case Nil ⇒ Nil
      case Cons(x, xs) ⇒  buf += f(x); loop(xs)
    }
    loop(l)
    List(buf.toSeq: _*)
  }

  def filter[A](l: List[A])(pred: A ⇒ Boolean): List[A] =
    foldRight(l, Nil: List[A])((a, acc) ⇒ if (pred(a)) Cons(a, acc) else acc)

  def flatMap[A,B](l: List[A])(f: A ⇒ List[B]): List[B] = concat(map(l)(f))

  def filter_1[A](l: List[A])(pred: A ⇒ Boolean): List[A] =
    flatMap(l)(a ⇒ if (pred(a)) Cons(a, Nil) else Nil)

  def combine(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (_, Nil) ⇒ Nil
    case (Nil, _) ⇒ Nil
    case (Cons(x, xs), Cons(y, ys)) ⇒ Cons(x + y, combine(xs, ys))
  }

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) ⇒ C): List[C] = (l, r) match {
    case (_, Nil) ⇒ Nil
    case (Nil, _) ⇒ Nil
    case (Cons(x, xs), Cons(y, ys)) ⇒ Cons(f(x, y), zipWith(xs, ys)(f))
  }

  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) ⇒ true
    case (Nil, _) ⇒ false
    case (Cons(x, xs), Cons(y, ys)) if x == y ⇒ startsWith(xs, ys)
    case _ ⇒ false
  }

  def hasSubsequence[A](l: List[A], r: List[A]): Boolean = {
    def loop[A](as: List[A]): Boolean = as match {
      case Nil ⇒ false
      case Cons(x, xs) if startsWith(as, r) ⇒ true
      case Cons(_, xs) ⇒ loop(xs)
    }
    loop(l)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[T](l: List[T]): List[T] = l match {
    case Nil ⇒ sys.error("tail of an empty list !")
    case Cons(_, t) ⇒ t
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil ⇒ Nil
    case Cons(_, _) if n == 0 ⇒ l
    case Cons(_, t) ⇒ drop(t, n - 1)
  }

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) ⇒ dropWhile(xs)(f)
    case _ ⇒ l
  }

  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil ⇒ sys.error("setHead on empty List")
    case Cons(_, t) ⇒ Cons(a, t)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil ⇒ sys.error("init on empty list !")
    case Cons(_, Nil) ⇒ Nil
    case Cons(x, xs) ⇒ Cons(x, init(xs))
  }

}
