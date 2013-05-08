package fpis.chap4.errorhandling

sealed trait Option[+A] {

  def map[B](f: A ⇒ B): Option[B] = this match {
    case None ⇒ None
    case Some(v) ⇒ Some(f(v))
  }

  def getOrElse[B >: A](default: ⇒ B): B = this match {
    case None ⇒ default
    case Some(v) ⇒ v
  }

  def flatMap[B](f: A ⇒ Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: ⇒ Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A ⇒ Boolean): Option[A] =
    flatMap(a ⇒ if (f(a)) Some(a) else None)

}
case class Some[+A](value: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m ⇒
      mean(xs map (x ⇒ math.pow(x - m, 2)))
    }

  def lift[A, B](f: A ⇒ B): Option[A] ⇒ Option[B] = _ map f

  import java.util.regex._

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException ⇒ None
    }

  def mkMatcher(pat: String): Option[String ⇒ Boolean] =
    pattern(pat) map (p ⇒ (s: String) ⇒ p.matcher(s).matches)

  def mkMatcher_1(pat: String): Option[String ⇒ Boolean] =
    for {
      p ← pattern(pat)
    } yield s ⇒ p.matcher(s).matches

  def doesMatch(pat: String, s: String): Option[Boolean] =
    for {
      p ← mkMatcher_1(pat)
    } yield p(s)

  def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] =
    for {
      p1 ← mkMatcher_1(pat1)
      p2 ← mkMatcher_1(pat2)
    } yield p1(s) && p2(s)

  def bothMatch_1(pat1: String, pat2: String, s: String): Option[Boolean] =
    mkMatcher_1(pat1) flatMap { p1 ⇒
      mkMatcher_1(pat2) map { p2 ⇒ p1(s) && p2(s) }
    }

  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) ⇒ C): Option[C] =
    for {
      a ← oa
      b ← ob
    } yield f(a, b)

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(mkMatcher(pat1), mkMatcher(pat2))((f1, f2) ⇒ f1(s) && f2(s))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])((o, acc) ⇒ map2(o, acc)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((o, acc) ⇒ map2(f(o), acc)(_ :: _))

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(v ⇒ v)
}
