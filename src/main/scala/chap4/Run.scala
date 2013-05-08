package fpis.chap4

import errorhandling._
import Option._

object Run extends App {
  println {
    mean(Seq(5, 10, 15))
  }
  println {
    sequence(Some(1) :: Some(2) :: Some(3) :: Nil)
  }
  println {
    sequence(Some(1) :: None :: Some(3) :: Nil)
  }
}
