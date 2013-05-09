package fpis.chap5.laziness

import Stream._

trait Stream[+A] {

  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  // strict ⇒ could blow the stack
  def toListRecursive: List[A] = uncons match {
    case None ⇒ Nil
    case Some((a, as)) ⇒ a :: as.toListRecursive
  }

  def toList: List[A] = {
    val buf = collection.mutable.ListBuffer[A]()
    @scala.annotation.tailrec
    def loop(sa: Stream[A]): Stream[A] = sa.uncons match {
      case None ⇒ empty
      case Some((a, as)) ⇒ buf += a; loop(as)
    }
    loop(this)
    buf.toList
  }

  def take(n: Int): Stream[A] = uncons match {
    case Some((a, as)) if n > 0 ⇒ cons(a, as.take(n - 1))
    case _ ⇒ empty
  }

  def takeWhile(p: A ⇒ Boolean): Stream[A] = uncons match {
    case Some((a, as)) if p(a) ⇒ cons(a, as.takeWhile(p))
    case _ ⇒ empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case None ⇒ z
      case Some((a, as)) ⇒ f(a, as.foldRight(z)(f))
    }

  def exists(p: A ⇒ Boolean): Boolean = foldRight(false)(p(_) || _)

  def forall(p: A ⇒ Boolean): Boolean = foldRight(false)(p(_) && _)

  def takeWhile_1(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) ⇒ if (p(a)) cons(a, acc) else empty)

  def map[B](f: A ⇒ B): Stream[B] =
    foldRight(empty[B])((a, acc) ⇒ cons(f(a), acc))

  def filter(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) ⇒ if (p(a)) cons(a, acc) else acc)

  def append[B >: A](bs: Stream[B]): Stream[B] = foldRight(bs)(cons(_, _))

  def flatMap[B](f: A ⇒ Stream[B]): Stream[B] =
    foldRight(empty[B])((a, acc) ⇒ f(a) append acc)

  def map_1[B](f: A ⇒ B): Stream[B] =
    unfold(this)( _.uncons map { case (h, t) ⇒ (f(h), t) })

  def take_1(n: Int): Stream[A] =
    unfold((this, n)){ case (as, i) ⇒
      if (i > 0) as.uncons map { case (h, t) ⇒ (h, (t, i - 1)) }
      else None
    }

  def takeWhile_2(p: A ⇒ Boolean): Stream[A] =
    unfold(this)(as ⇒ as.uncons filter { case (h, t) ⇒ p(h) })

}

object Stream {

  def empty[A]: Stream[A] = new Stream[A] { def uncons = None }

  def cons[A](hd: ⇒ A, tl: ⇒ Stream[A]): Stream[A] =
    new Stream[A] {
      override lazy val uncons = Some(hd, tl)
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def loop(cur: Int, next: Int): Stream[Int] =
      cons(cur, loop(next, cur + next))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S ⇒ Option[(A, S)]): Stream[A] =
    f(z) match {
      case None ⇒ empty
      case Some((a, s)) ⇒ cons(a, unfold(s)(f))
    }

  def fibs_1: Stream[Int] = unfold((0, 1)) { case (i1, i2) ⇒ Some((i1, (i2, i1 + i2))) }

  def from_1(n: Int): Stream[Int] = unfold(n)(i ⇒ Some((i, i + 1)))

  def constant_1[A](a: A): Stream[A] = unfold(a)(_ ⇒ Some((a, a)))

  val ones_1: Stream[Int] = unfold(1)(_ ⇒ Some(1, 1))

}
