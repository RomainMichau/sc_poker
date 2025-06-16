import Suit.{Clubs, Diamonds, Hearts, Spades}

object Common {

  val testDeck: Deck = Deck(List(
    Card(Spades, 1), Card(Hearts, 13), Card(Diamonds, 5), Card(Diamonds, 1), Card(Clubs, 11),
    Card(Clubs, 10), Card(Clubs, 9), Card(Diamonds, 4), Card(Diamonds, 11), Card(Diamonds, 12),
    Card(Clubs, 1), Card(Spades, 2), Card(Hearts, 1), Card(Hearts, 3), Card(Clubs, 2),
    Card(Hearts, 7), Card(Spades, 8), Card(Diamonds, 9), Card(Spades, 12), Card(Hearts, 6),
    Card(Clubs, 3), Card(Spades, 4), Card(Diamonds, 7), Card(Clubs, 7), Card(Hearts, 8),
    Card(Clubs, 12), Card(Hearts, 10), Card(Hearts, 2), Card(Spades, 9), Card(Diamonds, 8),
    Card(Clubs, 4), Card(Spades, 5), Card(Spades, 3), Card(Diamonds, 10), Card(Spades, 11),
    Card(Hearts, 12), Card(Spades, 6), Card(Hearts, 5), Card(Clubs, 13), Card(Hearts, 4),
    Card(Spades, 7), Card(Spades, 10), Card(Diamonds, 3), Card(Diamonds, 2), Card(Hearts, 9),
    Card(Clubs, 5), Card(Clubs, 6), Card(Spades, 13), Card(Hearts, 11), Card(Diamonds, 6),
    Card(Clubs, 8), Card(Diamonds, 13)
  ))

  val testPlayers: Seq[PlayerName] = Seq("p0", "p1", "p2", "p3", "p4")

  val startStack = 500
  val bigBlind = 10
  val smallBlind = 5
  val testGame = Game.newGame(testDeck, testPlayers, startStack, bigBlind, smallBlind)

  def mockActionRead(value: String): ActionReader = (* : String) => value

  def mockActionRead2(value1: String, value2: String): ActionReader = new ActionReader {
    var call = 0

    override def apply(blk: String): String = {
      call = call + 1
      if (call == 1) value1
      else if (call == 2) value2
      else throw new RuntimeException("unexpected call to mockActionRead2")
    }
  }

  def mockActionRead(values: Seq[String]): ActionReader = new ActionReader {
    var remainingValues = values

    override def apply(blk: String): String = remainingValues match {
      case v::rValues =>
        remainingValues = rValues
        println(v)
        v
    }
  }

}
