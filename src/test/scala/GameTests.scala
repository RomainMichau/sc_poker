import Common.{mockActionRead, mockActionRead2, testGame}
import com.rmichau.sc_poker.core_game.{ActionReader, Card, Game, PhaseAccounting, PlayerName}
import com.rmichau.sc_poker.core_game.Suit.{Clubs, Hearts, Spades}
import munit.FunSuite


class GameTests extends FunSuite {
  val p0 = testGame.players.head
  val p1 = testGame.players(1)
  val p2 = testGame.players(2)
  val p3 = testGame.players(3)
  val p4 = testGame.players(4)

  test("Game init works as expected") {
    assertEquals(testGame.deck.cards.size, 42)
    assert(testGame.phase.cards.isEmpty)
    assertEquals(testGame.players.head.hand, (Card(Spades, 1), Card(Hearts, 13)))
    assert(testGame.players.forall(_.stack == 500))
    assertEquals(testGame.bigBlind, 10)
    assertEquals(testGame.smallBlind, 5)
  }

//  test("Add card to community") {
//    val (g, _) = Game.addCardsToCommunity(2).run(testGame).value
//    assertEquals(g.communityCards, Seq(Card(Clubs, 1), Card(Spades, 2)))
//    assertEquals(g.deck.cards.length, 40)
//  }

  test("Pay blinds") {
    val (game, _) = Game.payBlinds().run(testGame).value
    assertEquals(game.players.head.stack, 495)
    assertEquals(game.players(1).stack, 490)
    assert(game.pot.forall { (pname, potContrib) =>
      (pname == p0.id && potContrib == 5) ||
        (pname == p1.id && potContrib == 10) || (!Set(p0.name, p1.name).contains(pname) && potContrib == 0)
    })
  }

  test("Get non next non folded") {
    val ngame = testGame.foldPlayer(1)
    assertEquals(ngame.getNextNonFolderPlayer(0).id, 2)
    val ngame2 = testGame.foldPlayer(1)
    assertEquals(ngame.getNextNonFolderPlayer(1).id, 2)
  }

  test("Player Fold") {
    val (game, _) = Game.playerMove(p0, PhaseAccounting.empty(testGame))(using mockActionRead("1")).run(testGame).value
    assert(game.players.head.isFold)
  }


  test("Player Call") {
    val gameSt = for {
      _ <- Game.payBlinds()
      turn = PhaseAccounting.initWithBlind(testGame)
      nturn <- Game.playerMove(p2, turn)(using mockActionRead("2"))
    } yield nturn

    val (game, turn) = gameSt.run(testGame).value
    assertEquals(game.players.head.stack, 495)
    assertEquals(game.players(1).stack, 490)
    assertEquals(game.players(2).stack, 490)

    assertEquals(turn.highestBet, Some(10))
    assertTurnBets(turn, Some(5), Some(10), Some(10), None, None)

    assertPot(game, 5, 10, 10, 0, 0)


  }

  test("Player raise") {
    val gameSt = for {
      _ <- Game.payBlinds()
      turn = PhaseAccounting.initWithBlind(testGame)
      nturn <- Game.playerMove(p2, turn)(using mockActionRead2("3", "100"))
    } yield nturn
    val (game, turn) = gameSt.run(testGame).value
    assertEquals(game.players.head.stack, 495)
    assertEquals(game.players(1).stack, 490)
    assertEquals(game.players(2).stack, 400)

    assertEquals(turn.highestBet, Some(100))
    assertTurnBets(turn, Some(5), Some(10), Some(100), None, None)
    assertPot(game, 5, 10, 100, 0, 0)
  }

  test("Play a phase") {
    given ActionReader = mockActionRead(Seq(
      // p0 SB: 5
      // p1 BB: 10
      "1", // p2 fold
      "2", // p3 call 10
      "2", // p4 call 10
      "3", "15", // p0 raise 15 Current bet is 20
      "1", // p1 fold
      
      "3", "20", // p3 raise 20
      "2", // p4 call10
      "1", // p0, fold
    ))

    val gameSt = for {
      _ <- Game.payBlinds()
      turn = PhaseAccounting.initWithBlind(testGame)
      nturn <- Game.playPhaseTurns(turn, testGame.players(2))
    } yield nturn
    val (game, turn) = gameSt.run(testGame).value
    assertFolded(game, Set(p1.name, p2.name, p0.name))
    assertEquals(game.potTotal, 90)

    assertPot(game, 20, 10, 0, 30, 30)
    assertTurnBets(turn, Some(20), Some(10), None, Some(30), Some(30))
    assertBet(game, 20, 10, 0, 30, 30)
    assertStack(game, 480, 490, 500, 470, 470)
  }

  private def assertTurnBets(turn: PhaseAccounting, p0b: Option[Int], p1b: Option[Int], p2b: Option[Int], p3b: Option[Int], p4b: Option[Int]): Unit = {
    assertEquals(turn.playerBets(0), p0b)
    assertEquals(turn.playerBets(1), p1b)
    assertEquals(turn.playerBets(2), p2b)
    assertEquals(turn.playerBets(3), p3b)
    assertEquals(turn.playerBets(4), p4b)
  }

  private def assertPot(game: Game, p0b: Int, p1b: Int, p2b: Int, p3b: Int, p4b: Int): Unit = {
    assertEquals(game.pot(0), p0b)
    assertEquals(game.pot(1), p1b)
    assertEquals(game.pot(2), p2b)
    assertEquals(game.pot(3), p3b)
    assertEquals(game.pot(4), p4b)
    assertEquals(game.potTotal, p1b + p2b + p3b + p4b + p0b)
  }

  private def assertStack(game: Game, p0b: Int, p1b: Int, p2b: Int, p3b: Int, p4b: Int): Unit = {
    assertEquals(game.players(0).stack, p0b)
    assertEquals(game.players(1).stack, p1b)
    assertEquals(game.players(2).stack, p2b)
    assertEquals(game.players(3).stack, p3b)
    assertEquals(game.players(4).stack, p4b)
  }

  private def assertBet(game: Game, p0b: Int, p1b: Int, p2b: Int, p3b: Int, p4b: Int): Unit = {
    assertEquals(game.players(0).totalBet, p0b)
    assertEquals(game.players(1).totalBet, p1b)
    assertEquals(game.players(2).totalBet, p2b)
    assertEquals(game.players(3).totalBet, p3b)
    assertEquals(game.players(4).totalBet, p4b)
  }

  private def assertFolded(game: Game, names: Set[PlayerName]): Unit = {
    assertEquals(game.players.filter(_.isFold).toSet.map(_.name), names)
  }
}
