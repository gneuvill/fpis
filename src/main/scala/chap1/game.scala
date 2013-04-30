package fpis.chap1

object Game {

  case class Player(name: String, score: Int)

  // ## BAD

  def printWinner(p: Player): Unit =
    println(p.name + " is the winner !")

  def badDeclareWinner(p1: Player, p2: Player): Unit =
    if (p1.score > p2.score) printWinner(p1)
    else printWinner(p2)

  // ## GOOD
  def winner(p1: Player, p2: Player): Player =
    if (p1.score > p2.score) p1 else p2

  def declareWinner(p1: Player, p2: Player): Unit =
    printWinner(winner(p1, p2))

}
