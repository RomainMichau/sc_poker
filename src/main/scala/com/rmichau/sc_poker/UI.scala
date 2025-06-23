package com.rmichau.sc_poker

import cats.data.State
import com.rmichau.sc_poker.core_game.{Game, PlayerId}

object UI {

  def clearConsole(): Unit = print("\u001b[2J")

  def printGame2(currentPlayer: Option[PlayerId] = None): State[Game, Unit] = for {
    _ <- State.inspect[Game, Unit] { game =>
      scala.Console.print("\u001b[2J")
      val players = game.players.map { p =>
        val pSt = s"${p.name}: (Stack: ${p.stack}€) (Cur Bet: ${p.totalBet}€)(${p.hand.toString()})"
        val pst2 = if(p.isFold) s"X $pSt" else pSt
        if(currentPlayer.contains(p.id)) s"-> $pst2" else pst2
      }.mkString("\n\n")
      val res =
        s"""
           |==========POKER=========
           |${game.phase.name}
           |Pot: ${game.potTotal}   Community Cards: ${game.phase.cards.mkString(" | ")}
           |
           |$players
           |""".stripMargin
      println(res)
    }
  } yield ()
}
