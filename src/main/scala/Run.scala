package fpis.chap5

import laziness._ 

object Run extends App {
  println {
    Stream(1, 2, 3, 4, 5).take(2).toList
  }
  println {
    Stream(1, 2, 3, 4, 5).takeWhile(_ != 4).toList
  }
  println {
    Stream(1, 2, 3, 4, 5).foldRight(0)(_ + _)
  }
  println {
    Stream(1, 2, 3, 4, 5).takeWhile_1(_ != 4).toList
  }
  println {
    Stream(1, 2, 3, 4, 5).filter(_ != 4).toList
  }
  println {
    Stream(1, 2, 3, 4, 5).append(Stream(6, 7, 8, 9, 10)).toList
  }
  println {
    Stream(1, 2, 3, 4, 5) flatMap { i ⇒ Stream(i, 28) } toList
  }
}
