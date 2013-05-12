package fpis.chap5

import laziness._ 
import Stream._

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
    Stream(1, 2, 3, 4, 5).flatMap(i ⇒ Stream(i, 28)).toList
  }
  println {
    constant("toto").take(5).toList
  }
  println {
    from(5).takeWhile(_ <= 15).toList
  }
  println {
    fibs.take(20).toList
  }
  println {
    fibs_1.take(20).toList
  }
  println {
    from_1(5).takeWhile(_ <= 15).toList
  }
  println {
    constant_1("toto").take(5).toList
  }
  println {
    Stream(1, 2, 3, 4, 5).take_1(2).toList
  }
  println {
    Stream(1, 2, 3, 4, 5).takeWhile_2(_ != 4).toList
  }
  println {
    Stream(1, 2, 3, 4) startsWith Stream(1, 2, 3)
  }
  println {
    (Stream(1, 2, 3, 4, 5).tails flatMap (s ⇒ s)).toList
  }
  println {
    Stream(23, 37, 3, 4, 5, 48) hasSubsequence Stream(3, 4, 5)
  }
  println {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList
  }
}
