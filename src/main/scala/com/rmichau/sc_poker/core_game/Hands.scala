package com.rmichau.sc_poker.core_game

import cats.{Comparison, Order}
import cats.data.NonEmptySeq
import cats.kernel.Order.given
import com.rmichau.sc_poker.core_game.Rank.{Ace, Five}

object HandHelper {
  /**
   * Returned sorted cards composing a straight if any
   * Hande low ace case. (Low ace will be returned as the last card)
   */
  def findBestStraight(cards: NonEmptySeq[Card]): Option[(Card, Card, Card, Card, Card)] = {
    val sortedCards = cards.sortBy(_.rank).sorted.reverse.distinctBy(_.rank).toSeq
    val straights = sortedCards.sliding(5)
      .filter(cs => cs.length == 5 && cs.max.rank.rank - cs.min.rank.rank == 4).toSeq

    val bestStraight = straights.reduceOption((cs1, cs2) => Order.max(cs1, cs2))
      .map {
        case c1 :: c2 :: c3 :: c4 :: c5 :: _ => (c1, c2, c3, c4, c5)
      }

    val maybeLowAceEdgeCase = sortedCards.filter(c => c.rank.rank <= 5 || c.rank == Ace) match {
      case c1 :: c2 :: c3 :: c4 :: c5 :: _ => Some(c2, c3, c4, c5, c1)
      case _ => None
    }

    bestStraight.orElse(maybeLowAceEdgeCase)
  }

  def findBestFlush(cards: NonEmptySeq[Card]): Option[(Card, Card, Card, Card, Card)] = cards
    .groupBy(_.suit)
    .collectFirst {
      case (_, suitedCards) if suitedCards.length >= 5 =>
        suitedCards.sorted.reverse match
          case NonEmptySeq(c1, c2 :: c3 :: c4 :: c5 :: _) => (c1, c2, c3, c4, c5)
    }

  object straightOrder extends  Order[Seq[Card]] {
    override def compare(x: Seq[Card], y: Seq[Card]): Int = {
      val xranks = x.map(_.rank)
      val yranks = y.map(_.rank)
      if(xranks.contains(Ace) && xranks.contains(Five)) -1
      else if(yranks.contains(Ace) && yranks.contains(Five)) 1
      else Order.compare(x, y)
    }
  }

  /**
   * Returned sorted cards composing a straight flush if any
   * Hande low ace case. (Low ace will be returned as the last card)
   */
  def findBestStraightFlush(cards: NonEmptySeq[Card]): Option[(Card, Card, Card, Card, Card)] = {

    cards.groupBy(_.suit)
      .values
      .toList
      .filter(_.length >= 5)
      .flatMap { c =>
        c.toSeq.sorted.reverse.sliding(5).filter(c => findBestStraight(NonEmptySeq.fromSeqUnsafe(c)).isDefined)
          .reduceOption((cs1, cs2) => Order.max(cs1, cs2)(using straightOrder))
      }
      .sorted
      .lastOption
      .map {
        case c1 :: c2 :: c3 :: c4 :: c5 :: _ => if(c1.rank == Ace && c2.rank == Five) (c2, c3, c4, c5, c1) else  (c1, c2, c3, c4, c5)
      }
  }


  /**
   * Return a desc sorted list of pairs
   */
  def findPairs(cards: NonEmptySeq[Card]): Seq[(Card, Card)] = cards.toSeq
    .groupBy(_.rank).values
    .toSeq
    .sorted.reverse
    .collect {
      case c1 :: c2 :: _ => (c1, c2)
    }

  /**
   * Return a desc sorted list of triple
   */
  def findTriples(cards: NonEmptySeq[Card]): Seq[(Card, Card, Card)] = cards.toSeq
    .groupBy(_.rank).values
    .toSeq
    .sorted.reverse
    .collect {
      case c1 :: c2 :: c3 :: _ => (c1, c2, c3)
    }

  /**
   * Return a desc sorted list of triple
   */
  def findFourOfaKinds(cards: NonEmptySeq[Card]): Seq[(Card, Card, Card, Card)] = cards
    .toSeq
    .groupBy(_.rank).values
    .toSeq
    .sorted.reverse
    .collect {
      case c1 :: c2 :: c3 :: c4 :: _ => (c1, c2, c3, c4)
    }
}

sealed trait Hand:
  def handRank: Int

object HighCard {
  def apply(r1: Rank, r2: Rank, r3: Rank, r4: Rank, r5: Rank): HighCard = {
    Seq(r1, r2, r3, r4, r5).sorted.reverse match {
      case sr1 :: sr2 :: sr3 :: sr4 :: sr5 :: _ => new HighCard(sr1, sr2, sr3, sr4, sr5)
    }
  }

  def from(cards: NonEmptySeq[Card]): HighCard = {
    cards.sorted.reverse.map(_.rank).toSeq match {
      case c1 :: c2 :: c3 :: c4 :: c5 :: _ => HighCard(c1, c2, c3, c4, c5)
    }
  }
}

class HighCard private(val r1: Rank, val r2: Rank, val r3: Rank, val r4: Rank, val r5: Rank) extends Hand:
  def handRank = 0

  private val ranks = List(r1, r2, r3, r4, r5)

  override def equals(obj: Any): Boolean = obj match
    case that: HighCard => this.ranks == that.ranks
    case _ => false

  override def hashCode(): Int = ranks.hashCode()

  override def toString: String = s"HighCard(${ranks.mkString(", ")})"

given Order[HighCard] = Order.by { h =>
  (h.r1, h.r2, h.r3, h.r4, h.r5)
}

object Pair {
  def apply(rank: Rank, k1: Rank, k2: Rank, k3: Rank): Pair = {
    Seq(k1, k2, k3).sorted.reverse match {
      case sk1 :: sk2 :: sk3 :: _ => new Pair(rank, sk1, sk2, sk3)
    }
  }

  def maybe(cards: NonEmptySeq[Card]): Option[Pair] = {
    HandHelper.findPairs(cards).headOption match {
      case Some((c1, c2)) =>
        cards.filterNot(Set(c1, c2).contains(_)).sorted.reverse match
          case k1 :: k2 :: k3 :: _ => Some(Pair(c1.rank, k1.rank, k2.rank, k3.rank))
      case None => None
    }
  }
}

class Pair private(val rank: Rank, val k1: Rank, val k2: Rank, val k3: Rank) extends Hand:
  def handRank = 1

  private val kickers = List(k1, k2, k3)

  override def equals(obj: Any): Boolean = obj match
    case that: Pair =>
      this.rank == that.rank && this.kickers == that.kickers
    case _ => false

  override def hashCode(): Int = 31 * rank.hashCode() + kickers.hashCode()

  override def toString: String = s"Pair($rank, $k1, $k2, $k3)"


given Order[Pair] = Order.by(p => (p.rank, p.k1, p.k2, p.k3))

object TwoPair {
  def apply(r1: Rank, r2: Rank, k1: Rank): TwoPair = new TwoPair(Order.max(r1, r2), Order.min(r2, r1), k1)

  def maybe(cards: NonEmptySeq[Card]): Option[TwoPair] = {
    val pairs = HandHelper.findPairs(cards)
    if (pairs.length >= 2) {
      pairs match {
        case (c1, c1b) :: (c2, c2b) :: _ =>
          val k = cards.filterNot(Set(c1, c1b, c2, c2b)).sorted.reverse.head
          Some(TwoPair(c1.rank, c2.rank, k.rank))
      }
    } else None
  }
}

class TwoPair private(val bigPair: Rank, val smallPair: Rank, val k1: Rank) extends Hand:
  def handRank = 2

  override def equals(obj: Any): Boolean = obj match
    case that: TwoPair =>
      this.bigPair == that.bigPair && this.smallPair == that.smallPair && this.k1 == that.k1
    case _ => false

  override def hashCode(): Int =
    31 * bigPair.hashCode() + 17 * smallPair.hashCode() + k1.hashCode()

  override def toString: String = s"TwoPair($bigPair, $smallPair, $k1)"


given Order[TwoPair] = Order.by { tp => (tp.bigPair, tp.smallPair, tp.k1) }


object ThreeOfAKind {
  def apply(rank: Rank, k1: Rank, k2: Rank): ThreeOfAKind = new ThreeOfAKind(rank, Order.max(k1, k2), Order.min(k2, k1))

  def maybe(cards: NonEmptySeq[Card]): Option[ThreeOfAKind] = {
    HandHelper.findTriples(cards).headOption.map {
      case (c1, c2, c3) =>
        val ks = cards.filterNot(Set(c1, c2, c3).contains(_))
          .sorted.reverse
          .take(2)
        ThreeOfAKind(c1.rank, ks.head.rank, ks(1).rank)
    }
  }
}

class ThreeOfAKind private(val rank: Rank, val k1: Rank, val k2: Rank) extends Hand:
  def handRank = 3

  private val kickers = List(k1, k2)

  override def equals(obj: Any): Boolean = obj match
    case that: ThreeOfAKind =>
      this.rank == that.rank && this.kickers == that.kickers
    case _ => false

  override def hashCode(): Int = 31 * rank.hashCode() + kickers.hashCode()

  override def toString: String = s"ThreeOfAKind($rank, ${kickers.mkString(", ")})"

given Order[ThreeOfAKind] = Order.by { tk => (tk.rank, tk.k1, tk.k2) }

object Straight {

  def maybe(cards: NonEmptySeq[Card]): Option[Straight] = {
    HandHelper.findBestStraight(cards).map {
      // handle low ace edge case
      case (c1, _, _, _, _) => Straight(c1.rank)
    }
  }
}

case class Straight(highestRank: Rank) extends Hand:
  def handRank = 4

given Order[Straight] = Order.by { st => st.highestRank }

object Flush {
  def apply(r1: Rank, r2: Rank, r3: Rank, r4: Rank, r5: Rank): Flush = {
    Seq(r1, r2, r3, r4, r5).sorted.reverse match {
      case sr1 :: sr2 :: sr3 :: sr4 :: sr5 :: _ => new Flush(sr1, sr2, sr3, sr4, sr5)
    }
  }

  def maybe(cards: NonEmptySeq[Card]): Option[Flush] = {
    HandHelper.findBestFlush(cards).map {
      case (c1, c2, c3, c4, c5) => Flush(c1.rank, c2.rank, c3.rank, c4.rank, c5.rank)
    }
  }
}

class Flush private(val r1: Rank, val r2: Rank, val r3: Rank, val r4: Rank, val r5: Rank) extends Hand:
  def handRank = 5

  private val ranks = List(r1, r2, r3, r4, r5)

  override def equals(obj: Any): Boolean = obj match
    case that: Flush => this.ranks == that.ranks
    case _ => false

  override def hashCode(): Int = ranks.hashCode()

  override def toString: String = s"Flush(${ranks.mkString(", ")})"

given Order[Flush] = Order.by { f => (f.r1, f.r2, f.r3, f.r4, f.r5) }

object FullHouse {
  def maybe(cards: NonEmptySeq[Card]): Option[FullHouse] = {
    HandHelper.findTriples(cards).map(_._1.rank).headOption
      .flatMap { trie =>
        HandHelper.findPairs(cards).map(_._1.rank).filterNot(_ == trie).headOption
          .map(p => FullHouse(trie, p))
      }
  }
}

case class FullHouse(threeOfAKind: Rank, pair: Rank) extends Hand:
  def handRank = 6

given Order[FullHouse] = Order.by { fh => (fh.threeOfAKind, fh.pair) }

object FourOfAKind {
  def maybe(cards: NonEmptySeq[Card]): Option[FourOfAKind] = {
    HandHelper.findFourOfaKinds(cards).headOption.map {
      case (c1, c2, c3, c4) =>
        val k = cards.filterNot(Set(c1, c2, c3, c4).contains(_)).sorted.reverse.head
        FourOfAKind(c1.rank, k.rank)
    }
  }
}

case class FourOfAKind(rank: Rank, k: Rank) extends Hand:
  def handRank = 7

given Order[FourOfAKind] = Order.by { fh => (fh.rank, fh.k) }

object StraightFlush {
  def maybe(cards: NonEmptySeq[Card]): Option[StraightFlush] = {
    HandHelper.findBestStraightFlush(cards).map(b => StraightFlush(b._1.rank))
  }
}

case class StraightFlush(highestRank: Rank) extends Hand:
  def handRank = 8

given Order[StraightFlush] = Order.by(sf => sf.highestRank)

def compareSubtype[A <: Hand](x: Hand, y: Hand)(using Order[A]): Int =
  Order[A].compare(x.asInstanceOf[A], y.asInstanceOf[A])

object Hands {
  given Order[Hand]:
    def compare(x: Hand, y: Hand): Int = (x, y) match
      case (_: HighCard, _: HighCard) => compareSubtype[HighCard](x, y)
      case (_: Pair, _: Pair) => compareSubtype[Pair](x, y)
      case (_: TwoPair, _: TwoPair) => compareSubtype[TwoPair](x, y)
      case (_: ThreeOfAKind, _: ThreeOfAKind) => compareSubtype[TwoPair](x, y)
      case (_: Straight, _: Straight) => compareSubtype[TwoPair](x, y)
      case (_: Flush, _: Flush) => compareSubtype[TwoPair](x, y)
      case (_: FullHouse, _: FullHouse) => compareSubtype[TwoPair](x, y)
      case (_: FourOfAKind, _: FourOfAKind) => compareSubtype[TwoPair](x, y)
      case (_: StraightFlush, _: StraightFlush) => compareSubtype[TwoPair](x, y)
      case _ => x.handRank - y.handRank
}

