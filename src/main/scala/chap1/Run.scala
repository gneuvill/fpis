package fpis.chap1

import Game._

object Run extends App {
    // #### RUN
  val sue = Player("Sue", 7)
  val bob = Player("Bob", 8)

  badDeclareWinner(sue, bob)

  val players = List(sue, bob, Player("Joe", 14))

  val win = players.reduceLeft(winner)

  printWinner(win)
}




