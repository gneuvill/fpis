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
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil ⇒ z
    case Cons(x, xs) ⇒ foldLeft(xs, f(z, x))(f)
  }

  def sumL(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def productL(doubles: List[Double]): Double = foldLeft(doubles, 1.0)(_ * _)

  def lengthL[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) ⇒ acc + 1)

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
    case Cons (_, Nil) ⇒ Nil
    case Cons(x, xs) ⇒ Cons(x, init(xs))
  }

}
