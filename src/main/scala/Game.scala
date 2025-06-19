import Game.Phase
import Game.Phase.{Flop, PreFlop, River, Turn}
import cats.data.{NonEmptySeq, State}

type PlayerName = String
type PlayerId = Int

trait ActionReader {
  def apply(ask: String = ""): String
}

case class Player(id: PlayerId, name: PlayerName, hand: (Card, Card), stack: Int, totalBet: Int, isFold: Boolean = false) {
  def updateStack(op: Int => Int): Player = this.copy(stack = op(stack))
  def updateBet(op: Int => Int): Player = this.copy(totalBet = op(totalBet))
}

object TurnAccounting {
  def empty(game: Game): TurnAccounting = TurnAccounting(game.players.map(_.id -> 0).toMap)

  def initWithBlind(game: Game): TurnAccounting = {
    val playersBet = game.players.map(_.id -> 0).toMap
      .updated(0, game.smallBlind)
      .updated(1, game.bigBlind)
    TurnAccounting(playersBet)
  }
}

case class TurnAccounting(playerBets: Map[PlayerId, Int]) {
  def highestBet = playerBets.values.max

  def getPlayerCall(player: PlayerId): Int = highestBet - playerBets(player)

  def registerBet(player: PlayerId, bet: Int): TurnAccounting = {
    val newPlayerBet = playerBets(player) + bet
    this.copy(playerBets = playerBets.updated(player, newPlayerBet))
  }

  def playerIsDone(playerId: PlayerId): Boolean = playerBets(playerId) == highestBet
}


object Game {

  def newGame(deck: Deck, playerNames: Seq[PlayerName], startStack: Int, bigBlind: Int, smallBlind: Int): Game = {
    val (hands, ndeck) = deck.dealHands(playerNames.length)
    val players = playerNames.zip(hands).zipWithIndex.map { case ((name, hand), id) => Player(id, name, hand, startStack, 0) }
    Game(ndeck, players, Seq.empty, bigBlind, smallBlind, players.map(_.id -> 0).toMap)
  }

  enum Phase {
    case PreFlop, Flop, Turn, River
  }

  def payBlinds(): State[Game, Unit] = for {
    g <- State.get
    _ <- payToPot(0, g.smallBlind)
    _ <- payToPot(1, g.bigBlind)
  } yield ()


  def addCardsToCommunity(nbCards: Int): State[Game, Unit] = {
    def loop(n: Int): State[Game, Unit] = {
      if (n <= 0) State.pure(())
      else for {
        game <- State.get[Game]
        (newDeck, card) = game.deck.pickCard()
        _ <- State.modify[Game](_.copy(
          deck = newDeck,
          communityCards = game.communityCards :+ card
        ))
        _ <- loop(n - 1)
      } yield ()
    }

    loop(nbCards)
  }

  private def payToPot(playerId: Int, amount: Int): State[Game, Unit] = {
    State.modify { g =>
      val player = g.players(playerId).updateStack(_ - amount).updateBet(_ + amount)
      val nplayers = g.players.updated(playerId, player)
      val cAmount = g.pot(playerId)
      g.copy(players = nplayers, pot = g.pot.updated(playerId, cAmount + amount))
    }
  }

  def play()(using actionReader: ActionReader): State[Game, Unit] = {
    for {
      _ <- playPhase(PreFlop)
      _ <- playPhase(Flop)
      _ <- playPhase(Turn)
      _ <- playPhase(River)
    } yield ()
  }
  
  def playPhase(phase: Phase)(using actionReader: ActionReader): State[Game, Unit] = {
    phase match {
      case PreFlop => for {
        _ <- State.modify[Game](g => g.setPhase(phase))
        _ <- Game.payBlinds()
        g <- State.get
        _ <- Game.playPhaseTurns(TurnAccounting.initWithBlind(g), true)
      } yield ()

      case Flop => for {
        _ <- State.modify[Game](g => g.setPhase(phase))
        _ <- Game.addCardsToCommunity(3)
        _ <- UI.printGame2()
        g <- State.get
        _ <- Game.playPhaseTurns(TurnAccounting.empty(g))
      } yield ()
      case Turn | River => for {
        _ <- State.modify[Game](g => g.setPhase(phase))
        _ <- Game.addCardsToCommunity(1)
        _ <- UI.printGame2()
        g <- State.get
        _ <- Game.playPhaseTurns(TurnAccounting.empty(g))
      } yield ()
    }
  }


  def playPhaseTurns(turn: TurnAccounting, skipBlinds: Boolean = false)(using actionReader: ActionReader): State[Game, TurnAccounting] = {
    for {
      _ <- UI.printGame2()
      nturn <- playTurn(turn, skipBlinds)
      game <- State.get[Game]
      nturn2 <- if (game.nonFoldedPlayer.forall(p => nturn.playerIsDone(p.id)))
        State.pure[Game, TurnAccounting](nturn)
      else
        playPhaseTurns(nturn)

    } yield (nturn2)
  }

  def playTurn(turn: TurnAccounting, skipBlinds: Boolean = false)(using actionReader: ActionReader): State[Game, TurnAccounting] = {
    for {
      game <- State.get[Game]
      players = if (!skipBlinds) game.nonFoldedPlayer else game.players.drop(2).filterNot(_.isFold)
      finalTurn <- players.foldLeft(State.pure[Game, TurnAccounting](turn)) {
        (accState, player) =>
          accState.flatMap(currentTurn => playerMove(player, currentTurn))
      }
    } yield finalTurn
  }

//  def playerMove(player: Player, turn: TurnAccounting)(using actionReader: ActionReader): State[Game, TurnAccounting] = {
//    val callAmount = turn.getPlayerCall(player.id)
//    println(s"${player.name} Next move: [1 Fold] [2 Call $callAmount] [3 Raise]\n") // Fixed typo (you had 2 "Fold")
//
//    val action = Action.readAction()
//
//    val print = for {
//      _ <- UI.printGame2(Some(player.id))
//    } yield ()
//
//    val res = action match {
//      case Fold() =>
//        for {
//          _ <- State.modify[Game](g => g.foldPlayer(player.id))
//        } yield turn
//
//      case Call() =>
//        for {
//          _ <- Game.payToPot(player.id, callAmount) // <-- modifies state
//        } yield turn.registerBet(player.id, callAmount)
//
//      case Raise(amount) =>
//        for {
//          _ <- Game.payToPot(player.id, amount)
//        } yield turn.registerBet(player.id, amount)
//    }
//    print.flatMap(_ => res)
//  }

  def playerMove(player: Player, turn: TurnAccounting)(using actionReader: ActionReader): State[Game, TurnAccounting] = {
    val callAmount = turn.getPlayerCall(player.id)

    for {
      _ <- UI.printGame2(Some(player.id)) // 👈 print game BEFORE asking

        _ <- State.inspect[Game, Unit] { game =>
            println(s"${player.name} Next move: [1 Fold] [2 Call $callAmount] [3 Raise]")}
      action <- State.pure(Action.readAction()) // 👈 delay user input until after printing

      updatedTurn <- action match {
        case Fold() =>
          State.modify[Game](_.foldPlayer(player.id)).map(_ => turn)

        case Call() =>
          Game.payToPot(player.id, callAmount).map(_ => turn.registerBet(player.id, callAmount))

        case Raise(amount) =>
          Game.payToPot(player.id, amount).map(_ => turn.registerBet(player.id, amount))
      }
    } yield updatedTurn
  }

}


case class Game(deck: Deck, players: Seq[Player], communityCards: Seq[Card],
                bigBlind: Int, smallBlind: Int, pot: Map[PlayerId, Int], phase: Phase = PreFlop) {
  def potTotal: Int = pot.map((_, v) => v).sum

  def foldPlayer(playerId: PlayerId): Game = {
    val player = players(playerId).copy(isFold = true)
    this.copy(players = players.updated(playerId, player))
  }

  def nonFoldedPlayer: Seq[Player] = this.players.filterNot(_.isFold)

  def setPhase(phase: Phase): Game = this.copy(phase = phase)

}
