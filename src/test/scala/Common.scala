import com.rmichau.sc_poker.core_game.{ActionReader, Card, Deck, Game, PlayerName, Rank}
import com.rmichau.sc_poker.core_game.Suit.{Clubs, Diamonds, Hearts, Spades}

object Common {

  val testDeck: Deck = Deck(List(
    Card(Spades, Rank(1)), Card(Hearts, Rank(13)), Card(Diamonds, Rank(5)), Card(Diamonds, Rank(1)), Card(Clubs, Rank(11)),
    Card(Clubs, Rank(10)), Card(Clubs, Rank(9)), Card(Diamonds, Rank(4)), Card(Diamonds, Rank(11)), Card(Diamonds, Rank(12)),
    Card(Clubs, Rank(1)), Card(Spades, Rank(2)), Card(Hearts, Rank(1)), Card(Hearts, Rank(3)), Card(Clubs, Rank(2)),
    Card(Hearts, Rank(7)), Card(Spades, Rank(8)), Card(Diamonds, Rank(9)), Card(Spades, Rank(12)), Card(Hearts, Rank(6)),
    Card(Clubs, Rank(3)), Card(Spades, Rank(4)), Card(Diamonds, Rank(7)), Card(Clubs, Rank(7)), Card(Hearts, Rank(8)),
    Card(Clubs, Rank(12)), Card(Hearts, Rank(10)), Card(Hearts, Rank(2)), Card(Spades, Rank(9)), Card(Diamonds, Rank(8)),
    Card(Clubs, Rank(4)), Card(Spades, Rank(5)), Card(Spades, Rank(3)), Card(Diamonds, Rank(10)), Card(Spades, Rank(11)),
    Card(Hearts, Rank(12)), Card(Spades, Rank(6)), Card(Hearts, Rank(5)), Card(Clubs, Rank(13)), Card(Hearts, Rank(4)),
    Card(Spades, Rank(7)), Card(Spades, Rank(10)), Card(Diamonds, Rank(3)), Card(Diamonds, Rank(2)), Card(Hearts, Rank(9)),
    Card(Clubs, Rank(5)), Card(Clubs, Rank(6)), Card(Spades, Rank(13)), Card(Hearts, Rank(11)), Card(Diamonds, Rank(6)),
    Card(Clubs, Rank(8)), Card(Diamonds, Rank(13))
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
