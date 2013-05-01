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
