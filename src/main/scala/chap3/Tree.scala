package fpis.chap3.datastructures

import scala.annotation._

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) ⇒ 1
    case Branch(l, r) ⇒ 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) ⇒ maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) ⇒ 0
    case Branch(l, r) ⇒ 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = t match {
    case Leaf(v) ⇒ Leaf(f(v))
    case Branch(l, r) ⇒ Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(l: A ⇒ B)(f: (B, B) ⇒ B): B = t match {
    case Leaf(v) ⇒ l(v)
    case Branch(left, right) ⇒ f(fold(left)(l)(f), fold(right)(l)(f))
  }

  def size_1[A](t: Tree[A]): Int = fold(t)(_ ⇒ 1)(1 + _ + _)

  def maximum_1(t: Tree[Int]): Int = fold(t)(i ⇒ i)(_ max _)

  def depth_1[A](t: Tree[A]): Int = fold(t)(_ ⇒ 0)((i1, i2) => 1 + (i1 max i2))

  def map_1[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = fold(t)(a ⇒ Leaf(f(a)): Tree[B])(Branch(_, _))

}
