package fpis.chap3

import datastructures._
import List._

object Run extends App {

  println {
    foldRight(List(1, 2, 3, 4, 5), 0)(_ + _)
  }
  println {
    lengthR(List((1 to 897): _*))
  }
  println {
    foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _)
  }
}
