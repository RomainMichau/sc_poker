import cats.data.NonEmptySeq
import munit.FunSuite
import cats.kernel.Order
import com.rmichau.sc_poker.core_game.{Flush, FourOfAKind, FullHouse, Hand, HandHelper, HighCard, Pair, Straight, StraightFlush, ThreeOfAKind, TwoPair, given}
import com.rmichau.sc_poker.core_game.Hands.given_Order_Hand


class HandsTests extends FunSuite {

  val p1: Pair = Pair(4, 1, 2, 3)
  val p2: Pair = Pair(3, 1, 2, 3)
  val p2_b: Pair = Pair(3, 2, 1, 3)
  val p3: Pair = Pair(5, 4, 5, 6)
  val p4: Pair = Pair(3, 2, 1, 4)

  test("Strong HighCard internal ordering") {
    val h1: Hand = HighCard(1, 5, 4, 9, 9)
    val h1_b: Hand = HighCard(9, 5, 4, 9, 1)
    val h2: Hand = HighCard(1, 5, 4, 9, 10)
    assert(Order.eqv(h1, h1_b))
    assert(Order.gt(h2, h1))
  }

  test("Test pair internal ordering") {
    assert(Order.gt(p1, p2))
    assert(Order.eqv(p2_b, p2))
    assert(Order.gt(p4, p2_b))
  }

  test("pair generation") {
    val p1 = Pair.maybe(NonEmptySeq(c"5H", Seq(c"5C", c"5D", c"2H", c"2D"))).get
    assertEquals(p1, Pair(5, 5,2,2))
    val noPair = Pair.maybe(NonEmptySeq(c"5H", Seq(c"2C", c"3D", c"4H", c"6D")))
    assert(noPair.isEmpty)
  }

  test("Two Pair internal ordering") {
    val tp: Hand = TwoPair(p1.rank, p2.rank, 5)
    val tp2: Hand = TwoPair(p3.rank, p2.rank, 5)
    val tp2_b: Hand = TwoPair(p2.rank, p3.rank, 5)
    val tp3: Hand = TwoPair(p3.rank, p2.rank, 6)
    assert(Order.gt(tp2, tp))
    assert(Order.gt(tp3, tp2))
    assert(Order.eqv(tp2, tp2_b))
  }

  test("twopair generation") {
    val tp1 = TwoPair.maybe(NonEmptySeq(c"5H", Seq(c"5C", c"5D", c"2H", c"2D"))).get
    assertEquals(tp1, TwoPair(5,2,5))
    val notp = TwoPair.maybe(NonEmptySeq(c"1H", Seq(c"3C", c"5D", c"2H", c"2D")))
    assert(notp.isEmpty)
  }

  test("Three of a kind internal ordering") {
    val t1 = ThreeOfAKind(5, 1, 2)
    val t2 = ThreeOfAKind(6, 1, 3)
    val t2_b = ThreeOfAKind(6, 1, 3)
    val t3 = ThreeOfAKind(6, 1, 4)
    assert(Order.gt(t2, t1))
    assert(Order.gt(t3, t2))
    assert(Order.eqv(t2_b, t2))
  }

  test("Three of a kind generation") {
    val t1 = ThreeOfAKind.maybe(NonEmptySeq(c"5H", Seq(c"5C", c"5D", c"2H", c"2D", c"2S"))).get
    assertEquals(t1, ThreeOfAKind(5, 2, 2))
    val t2 = ThreeOfAKind.maybe(NonEmptySeq(c"3H", Seq(c"2C", c"4D", c"5H", c"5D", c"5S"))).get
    assertEquals(t2, ThreeOfAKind(5, 4, 3))
    val notp = ThreeOfAKind.maybe(NonEmptySeq(c"1H", Seq(c"3C", c"5D", c"2H", c"2D")))
    assert(notp.isEmpty)
  }


  test("Straight internal ordering") {
    val s1 = Straight(8)
    val s2 = Straight(7)
    val s2_b = Straight(7)
    assert(Order.gt(s1, s2))
    assert(Order.eqv(s2_b, s2))
  }

  test("Straight generation") {
    val s1 = Straight.maybe(NonEmptySeq(c"5H", Seq(c"1D", c"2D", c"3H", c"4D", c"5D"))).get
    assertEquals(s1, Straight(5))
    val s2 = Straight.maybe(NonEmptySeq(c"6S", Seq(c"1D", c"2D", c"3H", c"4D", c"5D", c"7S"))).get
    assertEquals(s2, Straight(7))
    val empty = Straight.maybe(NonEmptySeq(c"1H", Seq(c"3C", c"5D", c"2H", c"2D")))
    assert(empty.isEmpty)
  }
  test("Flush internal ordering") {
    val f1 = Flush(10, 8, 6, 4, 2)
    val f2 = Flush(10, 8, 6, 4, 3)
    val f3 = Flush(10, 8, 6, 4, 3)
    assert(Order.gt(f2, f1))
    assert(Order.eqv(f2, f3))
  }

  test("Flush generation") {
    val f1 = Flush.maybe(NonEmptySeq(c"5D", Seq(c"1D", c"2D", c"2H", c"10D", c"11D"))).get
    assertEquals(f1, Flush(1, 11, 10, 5, 2))
    val f2 = Flush.maybe(NonEmptySeq(c"5D", Seq(c"1D", c"2D", c"12D", c"10D", c"11D"))).get
    assertEquals(f2, Flush(1, 12, 11, 10, 5))
    val notp = Flush.maybe(NonEmptySeq(c"1H", Seq(c"3C", c"5D", c"2H", c"2D")))
    assert(notp.isEmpty)
  }


  test("Full House internal ordering") {
    val fh1 = FullHouse(2, 4)
    val fh2 = FullHouse(3, 5)
    val fh3 = FullHouse(3, 5)
    val fh4 = FullHouse(4, 5)

    assert(Order.gt(fh2, fh1))
    assert(Order.eqv(fh2, fh3))
    assert(Order.gt(fh4, fh2)) // Pair kicker matters only if 3-of-a-kind are equal
  }

  test("Full house generation") {
    val f1 = FullHouse.maybe(NonEmptySeq(c"5D", Seq(c"5S", c"5C", c"2H", c"10D", c"10S"))).get
    assertEquals(f1, FullHouse(5, 10))
    val f2 = FullHouse.maybe(NonEmptySeq(c"5D", Seq(c"5S", c"5C", c"2H", c"10D", c"10S", c"10S"))).get
    assertEquals(f2, FullHouse(10, 5))
    val notp = FullHouse.maybe(NonEmptySeq(c"1H", Seq(c"3C", c"5D", c"2H", c"2D")))
    assert(notp.isEmpty)
  }


  test("Four of a Kind internal ordering") {
    val fk1 = FourOfAKind(6, 2)
    val fk2 = FourOfAKind(7, 2)
    val fk3 = FourOfAKind(7, 3)
    assert(Order.gt(fk2, fk1))
    assert(Order.lt(fk2, fk3)) // same quad, but kicker higher
  }

  test("Four of a kind generation") {
    val f1 = FourOfAKind.maybe(NonEmptySeq(c"5D", Seq(c"5S", c"5H", c"5C", c"10D", c"11S"))).get
    assertEquals(f1, FourOfAKind(5, 11))
    val f2 = FourOfAKind.maybe(NonEmptySeq(c"5D", Seq(c"5S", c"5C", c"5C", c"10D", c"11S", c"6S", c"6H", c"6C", c"6d"))).get
    assertEquals(f2, FourOfAKind(6, 11))
    val notp = FourOfAKind.maybe(NonEmptySeq(c"1H", Seq(c"3C", c"5D", c"2H", c"2D")))
    assert(notp.isEmpty)
  }

  test("Straight Flush internal ordering") {
    val sf1 = StraightFlush(9)
    val sf2 = StraightFlush(10)
    val sf3 = StraightFlush(10)
    val sf4 = StraightFlush(11)
    assert(Order.gt(sf2, sf1))
    assert(Order.eqv(sf3, sf2))
    assert(Order.gt(sf4, sf2))
  }

  test("Straight Flush generation") {
    val f1 = StraightFlush.maybe(NonEmptySeq(c"2D", Seq(c"3D", c"6D", c"4D", c"10D", c"5D"))).get
    assertEquals(f1, StraightFlush(6))
    val f2 = StraightFlush.maybe(NonEmptySeq(c"1D", Seq(c"8D", c"3D", c"4D", c"7D", c"2D", c"5D", c"6H", c"6C", c"6d"))).get
    assertEquals(f2, StraightFlush(8))
    val notp = StraightFlush.maybe(NonEmptySeq(c"1H", Seq(c"3C", c"5D", c"2H", c"2D")))
    assert(notp.isEmpty)
  }

  test("Different hand type comparisons (handRank ordering)") {
    val hands: List[Hand] = List(
      HighCard(2, 3, 4, 5, 6), // Rank 0
      Pair(4, 2, 3, 5), // Rank 1
      TwoPair(5, 3, 6), // Rank 2
      ThreeOfAKind(6, 2, 3), // Rank 3
      Straight(7), // Rank 4
      Flush(2, 4, 6, 8, 10), // Rank 5
      FullHouse(3, 8), // Rank 6
      FourOfAKind(9, 2), // Rank 7
      StraightFlush(10) // Rank 8
    )
    for {
      (h1, i) <- hands.zipWithIndex
      (h2, j) <- hands.zipWithIndex
      if i != j
    } {
      if i < j then
        assert(Order.lt(h1, h2), s"Expected ${h1.getClass.getSimpleName} < ${h2.getClass.getSimpleName}")
      else
        assert(Order.gt(h1, h2), s"Expected ${h1.getClass.getSimpleName} > ${h2.getClass.getSimpleName}")
    }
  }

  test("Find flush") {
    val flush = HandHelper.findBestFlush(NonEmptySeq(c"7D", Seq(c"4D", c"5D", c"6D", c"2D", c"8D"))).map(tupleToSeq).get
    assertEquals(flush.toSet, Seq(c"4D", c"5D", c"6D", c"7D", c"8D").toSet)
    val flush2 = HandHelper.findBestFlush(NonEmptySeq(c"7D", Seq(c"4D", c"5D", c"6H", c"2D", c"8D"))).map(tupleToSeq).get
    assertEquals(flush2.toSet, Seq(c"4D", c"5D", c"2D", c"7D", c"8D").toSet)
    val flush3 = HandHelper.findBestFlush(NonEmptySeq(c"7D", Seq(c"4H", c"5D", c"6H", c"2D", c"8D")))
    assert(flush3.isEmpty)
  }

  test("Find pair") {
    val pair = HandHelper.findPairs(NonEmptySeq(c"7D", Seq(c"7H", c"5D", c"6D", c"2D", c"8D")))
    assertEquals(pair.toSet, Set((c"7D", c"7H")))
    val pair2 = HandHelper.findPairs(NonEmptySeq(c"7D", Seq(c"7H", c"5D", c"6D", c"6C", c"8D")))
    assertEquals(pair2.toSet, Set((c"7D", c"7H"), (c"6D", c"6C")))
    val noPair = HandHelper.findPairs(NonEmptySeq(c"7D", Seq(c"1H", c"5D", c"2D", c"6C", c"8D")))
    assert(noPair.isEmpty)
  }

  test("find Staight") {
    val straight1 = HandHelper.findBestStraight(NonEmptySeq(c"9D", Seq(c"7H", c"5D", c"6D", c"4D", c"8D"))).map(tupleToSeq).get.toSet
    assertEquals(straight1, Set(c"7H", c"5D", c"6D", c"9D", c"8D"))
    val noStraight = HandHelper.findBestStraight(NonEmptySeq(c"9D", Seq(c"7H", c"5D", c"10D", c"4D", c"8D")))
    assert(noStraight.isEmpty)
  }

  test("find Staight Flush") {
    val straightF1 = HandHelper.findBestStraightFlush(NonEmptySeq(c"9D", Seq(c"7D", c"5D", c"6D", c"4D", c"8D"))).map(tupleToSeq).get.toSet
    assertEquals(straightF1, Set(c"7D", c"5D", c"6D", c"9D", c"8D"))
    val straightF2 = HandHelper.findBestStraightFlush(NonEmptySeq(c"9D", Seq(c"7D", c"5D", c"6D", c"4D", c"8D", c"8H", c"9H", c"12H", c"11H", c"10H"))).map(tupleToSeq).get.toSet
    assertEquals(straightF2, Set(c"8H", c"9H", c"10H", c"11H", c"12H"))
    val noStraightF = HandHelper.findBestStraight(NonEmptySeq(c"9D", Seq(c"7H", c"5D", c"10D", c"4D", c"8D")))
    assert(noStraightF.isEmpty)
    val noStraightF2 = HandHelper.findBestStraight(NonEmptySeq(c"9D", Seq(c"8D", c"11D", c"10D")))
    assert(noStraightF2.isEmpty)
  }

  def tupleToSeq[I](c: (I, I)): Seq[I] = Seq(c._1, c._2)

  def tupleToSeq[I](c: (I, I, I)): Seq[I] = Seq(c._1, c._2, c._3)

  def tupleToSeq[I](c: (I, I, I, I)): Seq[I] = Seq(c._1, c._2, c._3, c._4)

  def tupleToSeq[I](c: (I, I, I, I, I)): Seq[I] = Seq(c._1, c._2, c._3, c._4, c._5)

}
