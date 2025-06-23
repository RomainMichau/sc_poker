package com.rmichau.sc_poker.core_game

import cats.data.{NonEmptySeq, State}
import com.rmichau.sc_poker.core_game.Game.{Flop, Phase, PreFlop, River, Turn}
import com.rmichau.sc_poker.UI

type PlayerName = String
type PlayerId = Int

trait ActionReader {
  def apply(ask: String = ""): String
}

case class Player(id: PlayerId, name: PlayerName, hand: PlayerHand, stack: Int, totalBet: Int, isFold: Boolean = false) {
  def updateStack(op: Int => Int): Player = this.copy(stack = op(stack))

  def updateBet(op: Int => Int): Player = this.copy(totalBet = op(totalBet))
}

object PhaseAccounting {
  def empty(game: Game): PhaseAccounting = PhaseAccounting(game.players.map(_.id -> None).toMap)

  def initWithBlind(game: Game): PhaseAccounting = {
    val playersBet = game.players.map(_.id -> None).toMap
      .updated(0, Some(game.smallBlind))
      .updated(1, Some(game.bigBlind))
    PhaseAccounting(playersBet)
  }
}

case class PhaseAccounting(playerBets: Map[PlayerId, Option[Int]]) {
  def highestBet: Option[Int] = playerBets.values.max

  def playerHasBet(player: PlayerId): Boolean = playerBets(player).isDefined

  def getPlayerCall(player: PlayerId): Int = highestBet.getOrElse(0) - playerBets(player).getOrElse(0)

  def registerBet(player: PlayerId, bet: Int): PhaseAccounting = {
    val newPlayerBet = playerBets(player).getOrElse(0) + bet
    this.copy(playerBets = playerBets.updated(player, Some(newPlayerBet)))
  }

  def playerIsDone(playerId: PlayerId): Boolean = (playerBets(playerId), highestBet) match
    case (Some(pBet), Some(chighestBet)) => pBet == chighestBet
    case _ => false
}


object Game {

  def newGame(deck: Deck, playerNames: Seq[PlayerName], startStack: Int, bigBlind: Int, smallBlind: Int): Game = {
    val (hands, ndeck) = deck.dealHands(playerNames.length)
    val players = playerNames.zip(hands).zipWithIndex.map { case ((name, hand), id) => Player(id, name, hand, startStack, 0) }
    Game(ndeck, players, bigBlind, smallBlind, players.map(_.id -> 0).toMap)
  }

  sealed trait Phase {
    def cards: Seq[Card]
    def name: String
  }

  case class PreFlop() extends Phase {
    override def cards: Seq[Card] = Seq()

    override def name: PlayerName = "PreFlop"
  }

  case class Flop(c1: Card, c2: Card, c3: Card) extends Phase {
    override def cards: Seq[Card] = Seq(c1, c2, c3)
    override def name: PlayerName = "Flop"
    
  }

  case class Turn(c1: Card, c2: Card, c3: Card, c4: Card) extends Phase {
    override def cards: Seq[Card] = Seq(c1, c2, c3, c4)
    override def name: PlayerName = "Turn"
    
  }

  case class River(c1: Card, c2: Card, c3: Card, c4: Card, c5: Card) extends Phase {
    override def cards: Seq[Card] = Seq(c1, c2, c3, c4, c5)
    override def name: PlayerName = "River"
  }

  def payBlinds(): State[Game, Unit] = for {
    g <- State.get
    _ <- payToPot(0, g.smallBlind)
    _ <- payToPot(1, g.bigBlind)
  } yield ()


  //  def addCardsToCommunity(nbCards: Int): State[Game, Unit] = {
  //    def loop(n: Int): State[Game, Unit] = {
  //      if (n <= 0) State.pure(())
  //      else for {
  //        game <- State.get[Game]
  //        (newDeck, card) = game.deck.pickCard()
  //        _ <- State.modify[Game](_.copy(
  //          deck = newDeck,
  //          communityCards = game.communityCards :+ card
  //        ))
  //        _ <- loop(n - 1)
  //      } yield ()
  //    }
  //
  //    loop(nbCards)
  //  }

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
      _ <- playPhase()
      hasANextPhase <- nextPhase()
      _ <- if (hasANextPhase) play() else State.pure[Game, Unit](())
    } yield ()
  }

  def playPhase()(using actionReader: ActionReader): State[Game, Unit] = for {
    g <- State.get[Game]
    _ <- g.phase match {
      case PreFlop() => for {
        _ <- Game.payBlinds()
        g <- State.get
        _ <- Game.playPhaseTurns(PhaseAccounting.initWithBlind(g), g.players(2))
      } yield ()

      case Flop(_, _, _) => for {
        _ <- UI.printGame2()
        g <- State.get
        _ <- Game.playPhaseTurns(PhaseAccounting.empty(g), g.players(0))
      } yield ()
      case Turn(_, _, _, _) | River(_, _, _, _, _) => for {
        _ <- UI.printGame2()
        g <- State.get
        _ <- Game.playPhaseTurns(PhaseAccounting.empty(g), g.players(0))
      } yield ()
    }
  } yield ()


  def playPhaseTurns(phaseAccounting: PhaseAccounting, player: Player)(using actionReader: ActionReader): State[Game, PhaseAccounting] = {
    for {
      nphaseAcc <- playerMove(player, phaseAccounting)
      g <- State.get[Game]
      nphaseAcc2 <- if (g.nonFoldedPlayer.forall(p => nphaseAcc.playerIsDone(p.id))) State.pure[Game, PhaseAccounting](nphaseAcc)
      else playPhaseTurns(nphaseAcc, g.getNextNonFolderPlayer(player.id))
    } yield (nphaseAcc2)
  }

  def playerMove(player: Player, turn: PhaseAccounting)(using actionReader: ActionReader): State[Game, PhaseAccounting] = {
    val callAmount = turn.getPlayerCall(player.id)

    for {
      _ <- UI.printGame2(Some(player.id)) // 👈 print game BEFORE asking

      _ <- State.inspect[Game, Unit] { game =>
        println(s"${player.name} Next move: [1 Fold] [2 Call $callAmount] [3 Raise]")
      }
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

  def nextPhase(): State[Game, Boolean] = for {
    g <- State.get[Game]
    res = g.phase match {
      case River(_, _, _, _, _) => false
      case _ => true
    }
    _ <- State.modify[Game] { game =>
      game.phase match {
        case PreFlop() =>
          val (d2, c1) = game.deck.pickCard()
          val (d3, c2) = d2.pickCard()
          val (d4, c3) = d3.pickCard()
          game.copy(deck = d4, phase = Flop(c1, c2, c3))
        case Flop(c1, c2, c3) =>
          val (d2, c4) = game.deck.pickCard()
          game.copy(deck = d2, phase = Turn(c1, c2, c3, c4))
        case Turn(c1, c2, c3, c4) =>
          val (d2, c5) = game.deck.pickCard()
          game.copy(deck = d2, phase = River(c1, c2, c3, c4, c5))
        case _ => game
      }
    }
  } yield res
}


case class Game(deck: Deck, players: Seq[Player],
                bigBlind: Int, smallBlind: Int, pot: Map[PlayerId, Int], phase: Phase = PreFlop()) {
  def potTotal: Int = pot.map((_, v) => v).sum

  def foldPlayer(playerId: PlayerId): Game = {
    val player = players(playerId).copy(isFold = true)
    this.copy(players = players.updated(playerId, player))
  }

  def nonFoldedPlayer: Seq[Player] = this.players.filterNot(_.isFold)

  //  def setPhase(phase: Phase): Game = this.copy(phase = phase)


  // TODO return an Option
  def getNextNonFolderPlayer(currentPlayer: PlayerId): Player = (players.dropWhile(_.id != currentPlayer).tail ++ nonFoldedPlayer)
    .find(!_.isFold).get

}
