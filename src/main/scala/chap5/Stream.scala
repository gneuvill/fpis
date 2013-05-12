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

  def forAll(p: A ⇒ Boolean): Boolean = foldRight(true)(p(_) && _)

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
    unfold(this)(_.uncons map { case (h, t) ⇒ (f(h), t) })

  def take_1(n: Int): Stream[A] =
    unfold((this, n)) {
      case (as, i) ⇒
        if (i > 0) as.uncons map { case (h, t) ⇒ (h, (t, i - 1)) }
        else None
    }

  def takeWhile_2(p: A ⇒ Boolean): Stream[A] =
    unfold(this)(as ⇒ as.uncons filter { case (h, t) ⇒ p(h) })

  def zipWith[B, C](sb: Stream[B])(f: (A, B) ⇒ C): Stream[C] =
    unfold((this, sb)) {
      case (as, bs) ⇒ for {
        (a, ta) ← as.uncons
        (b, tb) ← bs.uncons
      } yield (f(a, b), (ta, tb))
    }

  def zip[B](bs: Stream[B]): Stream[(A, B)] = zipWith(bs)((_, _))

  def zipWithAllDumb[B, C](sb: Stream[B])(f: (Option[A], Option[B]) ⇒ C): Stream[C] =
    unfold((this, sb)) {
      case (as, bs) ⇒
        (as.uncons, bs.uncons) match {
          case (Some((a, ta)), Some((b, tb))) ⇒ Some((f(Some(a), Some(b)), (ta, tb)))
          case (Some((a, ta)), None) ⇒ Some((f(Some(a), None), (ta, empty)))
          case (None, Some((b, tb))) ⇒ Some((f(None, Some(b)), (empty, tb)))
          case _ ⇒ None
        }
    }

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = {
    val a = this map (Some(_)) append (constant(None))
    val b = s2 map (Some(_)) append (constant(None))
    unfold((a, b)) {
      case (s1, s2) if s1.isEmpty && s2.isEmpty => None
      case (s1, s2) => {
        val (h1, t1) = s1.uncons.get
        val (h2, t2) = s2.uncons.get
        Some((f(h1, h2), (t1, t2)))
      }
    }
  }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = zipWithAll(bs)((_, _))

  def startsWith[B >: A](bs: Stream[B]): Boolean =
    zipAll(bs).takeWhile(!_._2.isEmpty) forAll {
      case (Some(h), Some(h2)) if h == h2 => true
      case _ => false
    }

  def tails: Stream[Stream[A]] = unfold(this)(_.uncons map { case (h, t) ⇒ (cons(h, t), t) })

  def hasSubsequence[B >: A](bs: Stream[B]): Boolean = tails exists (_ startsWith bs)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight(Stream(z))((a, bs) ⇒ bs.uncons match {
      case None ⇒ empty
      case Some((b, t)) ⇒ cons(f(a, b), bs)
    })

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
