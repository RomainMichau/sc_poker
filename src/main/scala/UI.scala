import cats.data.State

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
           |${game.phase}
           |Pot: ${game.potTotal}   Community Cards: ${game.communityCards.mkString(" | ")}
           |
           |$players
           |""".stripMargin
      println(res)
    }
  } yield ()

  //    val communityStr = state.communityCards.map(_.toString).mkString(" ")
//    val playersStr = state.players.zipWithIndex.map { case (p, i) =>
//      val cardsStr = p.hand.toString()
////      val activeMarker = if (i == state.currentPlayer) " <-- Your move" else ""
//
//      f"${p.name}%-10s Stack: ${p.stack}%4d  $cardsStr"
//    }.mkString("\n")
//
//    val res = s"""
//       |=== POKER GAME ===
//       |
//       |Pot: ${state.pot}
//       |Community Cards: $communityStr
//       |
//       |$playersStr
//       |""".stripMargin
//    println(res)
//  }


}
