package fpis.chap6

import state.RNG._

object Run extends App {
  var r =  double(simple(120L))._2
  for (_ ← 0 until 1000) {
    val p = double(r)
    r = p._2
    //println(p._1)
  }

  println {
    ints(10)(simple(2L))
  }
  println {
    ints_1(10)(simple(2L))
  }

}
