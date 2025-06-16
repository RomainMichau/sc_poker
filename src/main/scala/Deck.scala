import cats.data.State

import scala.util.Random

enum Suit {
  case Hearts, Diamonds, Clubs, Spades

  override def toString: String = this match {
    case Hearts => "♥"
    case Diamonds => "♦"
    case Clubs => "♣"
    case Spades => "♠"
  }
}

case class Card(suit: Suit, rank: Int) {
  override def toString: String = rank match {
    case 11 => s"J$suit"
    case 12 => s"Q$suit"
    case 13 => s"K$suit"
    case v => s"$v$suit"
  }
}

type Hand = (Card, Card)

object Deck {
  def shuffled(): Deck = {
    val cards = Random.shuffle(for {
      rank <- 1 to 13
      suit <- Suit.values
    } yield Card(suit, rank)).toList
    Deck(cards)
  }


}
case class Deck(cards: Seq[Card]) {
  def pickCard(): (Deck, Card) =
    cards match {
      case card :: remaining => (Deck(remaining), card)
  }

  def dealHands(nbPlayers: Int): (Seq[Hand], Deck) = {
    val (toDeal, remaining) = cards.splitAt(nbPlayers * 2)
    val hands = toDeal.grouped(2).map(s => (s.head, s(1))).toSeq
    val newDeck = Deck(remaining)
    (hands, newDeck)
  }
}
