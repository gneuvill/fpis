package fpis.chap4.errorhandling

sealed trait Either[+E, +A] {

  def map[B](f: A ⇒ B): Either[E, B] = this match {
    case Left(e) ⇒ Left(e)
    case Right(v) ⇒ Right(f(v))
  }

  def flatMap[EE >: E, B](f: A ⇒ Either[EE, B]): Either[EE, B] = this match {
    case Left(e) ⇒ Left(e)
    case Right(v) ⇒ f(v)
  }

  def orElse[EE >: E, B >: A](b: ⇒ Either[EE, B]): Either[EE, B] = this match {
    case Left(_) ⇒ b
    case Right(v) ⇒ Right(v)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      v1 ← this
      v2 ← b
    } yield f(v1, v2)
}
case class Left[+E](error: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] =  
    a.foldRight(Right(Nil): Either[E, List[B]])((ea, acc) ⇒ f(ea).map2(acc)(_ :: _))

  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = traverse(a)(ei ⇒ ei)
}


















