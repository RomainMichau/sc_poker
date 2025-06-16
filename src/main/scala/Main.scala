import cats.data.State

val testPlayers: Seq[PlayerName] = Seq("p0", "p1", "p2", "p3", "p4")

val startStack = 500
val bigBlind = 10
val smallBlind = 5
val testGame = Game.newGame(Deck.shuffled(), testPlayers, startStack, bigBlind, smallBlind)

object Main {
  given ActionReader = ConsoleActionReader
  def main(args: Array[String]): Unit = {
    val  res = Game.play().run(testGame).value
    println(res)
  }
}