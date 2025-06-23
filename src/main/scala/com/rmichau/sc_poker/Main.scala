package com.rmichau.sc_poker

import cats.data.State
import com.rmichau.sc_poker.core_game.{ActionReader, Deck, Game, PlayerName}

val testPlayers: Seq[PlayerName] = Seq("p0", "p1", "p2", "p3", "p4")

val startStack = 500
val bigBlind = 10
val smallBlind = 5
val initGame: Game = Game.newGame(Deck.shuffled(), testPlayers, startStack, bigBlind, smallBlind)

object Main {
  given ActionReader = ConsoleActionReader
  def main(args: Array[String]): Unit = {
    Game.play().run(initGame).value
  }
}