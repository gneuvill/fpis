package fpis.chap3

import datastructures._
import List._
import Tree.{map ⇒ tmap, map_1 ⇒ tmap_1, _}
 
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
  println {
    foldLeft2(List(1, 2, 3, 4, 5), 0)(_ + _)
  }
  println {
    reverse(List(1, 2, 3, 4, 5))
  }
  println("*******************")
  println {
    foldLeft(List(1, 2, 3, 4, 5), Nil: List[Int])((b, a) ⇒ Cons(a, b))
  }
  println {
    foldLeft2(List(1, 2, 3, 4, 5), Nil: List[Int])((b, a) ⇒ Cons(a, b))
  }
  println {
    append2(List(1, 2, 3, 4), List(5, 6))
  }
  println {
    concat(List(List(1, 3), List(2, 4), List(3, 5)))
  }
  println {
    add1(List(1, 2, 3, 4, 5))
  }
  println {
    doubleToString(List(1.2, 2.3, 3.4, 5.6))
  }
  println {
    map(List(1, 2, 3, 4, 5))(_ * 2)
  }
  println {
    filter(List(1, 2, 3, 4, 5, 6, 7, 8))(_ % 2 != 0)
  }
  println {
    filter_1(List(1, 2, 3, 4, 5, 6, 7, 8))(_ % 2 != 0)
  }
  println {
    combine(List(1, 2, 3), List(4, 5, 6))
  }
  println {
    hasSubsequence(List(1, 2, 3, 4, 5), List(2, 3, 4))
  }
  println("*********************")
  println {
    size(Branch(Branch(Leaf("toto"), Leaf("tata")), Leaf("titi")))
  }
  println {
    maximum(Branch(Branch(Leaf(2), Leaf(7)), Leaf(5)))
  }
  println {
    depth(Branch(Branch(Leaf("toto"), Branch(Leaf("tata"), Leaf("tutu"))), Leaf("titi")))
  }
  println {
    tmap(Branch(Branch(Leaf(2), Leaf(7)), Leaf(5)))(_ + 1)
  }
  println("*********************")
  println {
    size_1(Branch(Branch(Leaf("toto"), Leaf("tata")), Leaf("titi")))
  }
  println {
    maximum_1(Branch(Branch(Leaf(2), Leaf(7)), Leaf(5)))
  }
  println {
    depth_1(Branch(Branch(Leaf("toto"), Branch(Leaf("tata"), Leaf("tutu"))), Leaf("titi")))
  }
  println {
    tmap_1(Branch(Branch(Leaf(2), Leaf(7)), Leaf(5)))(_ + 1)
  }

}
