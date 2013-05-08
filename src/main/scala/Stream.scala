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

  def forall(p: A ⇒ Boolean): Boolean =  foldRight(false)(p(_) && _)

  def takeWhile_1(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) ⇒ if (p(a)) cons(a, acc) else empty)

  def map[B](f: A ⇒ B): Stream[B] =
    foldRight(empty[B])((a, acc) ⇒ cons(f(a), acc))

  def filter(p: A ⇒ Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) ⇒ if (p(a)) cons(a, acc) else acc)

  def append[B >: A](bs: Stream[B]): Stream[B] = foldRight(bs)(cons(_, _))

  def flatMap[B](f: A ⇒ Stream[B]): Stream[B] =
    foldRight(empty[B])((a, acc) ⇒ f(a) append acc)

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
}



















