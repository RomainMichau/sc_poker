package com.rmichau.sc_poker.core_game

import cats.Order
import cats.data.State

import scala.util.Random

object Rank {
  def apply(n: Int): Rank = n match
    case 2 => Rank.Two
    case 3 => Rank.Three
    case 4 => Rank.Four
    case 5 => Rank.Five
    case 6 => Rank.Six
    case 7 => Rank.Seven
    case 8 => Rank.Eight
    case 9 => Rank.Nine
    case 10 => Rank.Ten
    case 11 => Rank.Jack
    case 12 => Rank.Queen
    case 13 => Rank.King
    case 14 | 1 => Rank.Ace
    case _ => throw new IllegalArgumentException(s"Invalid rank number: $n")

  given orderRankAceHigh: Order[Rank] = Order.by(_.rank)
  given intToRank: Conversion[Int, Rank] = (x: Int) => Rank(x)
}
enum Rank(val rank: Int):
  case Two extends Rank(2)
  case Three extends Rank(3)
  case Four extends Rank(4)
  case Five extends Rank(5)
  case Six extends Rank(6)
  case Seven extends Rank(7)
  case Eight extends Rank(8)
  case Nine extends Rank(9)
  case Ten extends Rank(10)
  case Jack extends Rank(11)
  case Queen extends Rank(12)
  case King extends Rank(13)
  case Ace extends Rank(14)

  override def toString: String = this match
    case Two => "2"
    case Three => "3"
    case Four => "4"
    case Five => "5"
    case Six => "6"
    case Seven => "7"
    case Eight => "8"
    case Nine => "9"
    case Ten => "10"
    case Jack => "J"
    case Queen => "Q"
    case King => "K"
    case Ace => "A"


enum Suit {
  case Hearts, Diamonds, Clubs, Spades

  override def toString: String = this match {
    case Hearts => "♥"
    case Diamonds => "♦"
    case Clubs => "♣"
    case Spades => "♠"
  }
}

given Order[Suit] = Order.by(_.toString)

case class Card(suit: Suit, rank: Rank) {
  override def toString: String = s"$rank$suit"
}

given Order[Card] = Order.by(_.rank)

implicit class CardStringContext(val sc: StringContext) extends AnyVal {
  def c(args: Any*): Card = {
    val input = sc.parts.mkString // e.g., "3heart"
    parseCard(input) // custom parser below
  }

  private def parseCard(s: String): Card = {
    val rank = s.dropRight(1).toUpperCase() match
      case "2" => Rank.Two
      case "3" => Rank.Three
      case "4" => Rank.Four
      case "5" => Rank.Five
      case "6" => Rank.Six
      case "7" => Rank.Seven
      case "8" => Rank.Eight
      case "9" => Rank.Nine
      case "10" => Rank.Ten
      case "J" | "11" => Rank.Jack
      case "Q" | "12" => Rank.Queen
      case "K" | "13" => Rank.King
      case "A" | "1" | "14" => Rank.Ace
      case wut => throw new IllegalArgumentException(s"Unexpected rank $wut")
    val suitStr = s.dropWhile(_.isDigit).toLowerCase
    val suit = suitStr match {
      case "♥" | "h" => Suit.Hearts
      case "♠" | "s" => Suit.Spades
      case "♣" | "c" => Suit.Clubs
      case "♦" | "d" => Suit.Diamonds
      case _ => throw new IllegalArgumentException(s"Invalid suit: $suitStr")
    }

    Card(suit, rank)
  }
}

type PlayerHand = (Card, Card)

object Deck {
  def shuffled(): Deck = {

    val cards = Random.shuffle(for {
      rank <- 1 to 13
      suit <- Suit.values
    } yield Card(suit, Rank(rank))).toList
    Deck(cards)
  }


}

case class Deck(cards: Seq[Card]) {
  def pickCard(): (Deck, Card) =
    cards match {
      case card :: remaining => (Deck(remaining), card)
    }

  def dealHands(nbPlayers: Int): (Seq[PlayerHand], Deck) = {
    val (toDeal, remaining) = cards.splitAt(nbPlayers * 2)
    val hands = toDeal.grouped(2).map(s => (s.head, s(1))).toSeq
    val newDeck = Deck(remaining)
    (hands, newDeck)
  }
}
