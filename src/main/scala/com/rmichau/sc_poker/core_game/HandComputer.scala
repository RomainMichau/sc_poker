package com.rmichau.sc_poker.core_game

import cats.data.{NonEmptySeq, State}
import com.rmichau.sc_poker.core_game.Game.River
import cats.kernel.Order.given
import com.rmichau.sc_poker.core_game.Hands.given
object HandComputer {
  def findBestHand(playerHand: PlayerHand, river: River): Hand = {
    val cards = (playerHand, river) match
      case ((c1, c2), River(c3, c4, c5, c6, c7)) => NonEmptySeq(c1, Seq(c2, c3, c4, c5, c6, c7))
    StraightFlush.maybe(cards)
      .orElse(FourOfAKind.maybe(cards))
      .orElse(FullHouse.maybe(cards))
      .orElse(Flush.maybe(cards))
      .orElse(Straight.maybe(cards))
      .orElse(ThreeOfAKind.maybe(cards))
      .orElse(TwoPair.maybe(cards))
      .orElse(Pair.maybe(cards))
      .getOrElse(HighCard.from(cards))

  }

  def findBestPlayerHand(player: Seq[Player], river: River): Seq[(Player, Hand)] = {
    val bestHandPerPlayer = player.map(p => p -> findBestHand(p.hand, river))
    bestHandPerPlayer.sortBy(_._2).reverse
  }
}
