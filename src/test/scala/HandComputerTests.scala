import com.rmichau.sc_poker.core_game.Game.River
import com.rmichau.sc_poker.core_game.Rank.given
import com.rmichau.sc_poker.core_game.{CardStringContext, Flush, FourOfAKind, FullHouse, HandComputer, HighCard, Pair, Straight, StraightFlush, ThreeOfAKind, TwoPair}
import munit.FunSuite

class HandComputerTests extends FunSuite {
  test("test high card") {
    val hand = HandComputer.findBestHand((c"2C", c"9D"), River(c"3S", c"5H", c"6D", c"8C", c"10H"))
    assertEquals(hand, HighCard(10, 9, 8, 6, 5))
  }

  test("test pair") {
    val hand = HandComputer.findBestHand((c"5S", c"3S"), River(c"10D", c"5D", c"11H", c"1H", c"2D"))
    assertEquals(hand, Pair(5, 1, 11, 10))
  }

  test("test two pair") {
    val hand = HandComputer.findBestHand((c"5S", c"3S"), River(c"3D", c"5D", c"11H", c"1H", c"2D"))
    assertEquals(hand, TwoPair(5, 3, 1))
  }

  test("test three of a kind") {
    val hand = HandComputer.findBestHand((c"7S", c"7D"), River(c"7H", c"2C", c"3D", c"4S", c"10H"))
    assertEquals(hand, ThreeOfAKind(7, 10, 4))
  }

  test("test straight") {
    val hand = HandComputer.findBestHand((c"2C", c"3D"), River(c"4S", c"5H", c"6D", c"8C", c"10H"))
    assertEquals(hand, Straight(6))
  }

  test("test flush") {
    val hand = HandComputer.findBestHand((c"2H", c"4H"), River(c"6H", c"8H", c"10H", c"3S", c"7C"))
    assertEquals(hand, Flush(10, 8, 6, 4, 2))
  }

  test("test full house") {
    val hand = HandComputer.findBestHand((c"3S", c"3H"), River(c"3D", c"2H", c"2D", c"5S", c"6S"))
    assertEquals(hand, FullHouse(3, 2))
  }

  test("test four of a kind") {
    val hand = HandComputer.findBestHand((c"9S", c"9D"), River(c"9H", c"9C", c"2D", c"5S", c"6S"))
    assertEquals(hand, FourOfAKind(9, 6))
  }

  test("test straight flush") {
    val hand = HandComputer.findBestHand((c"2H", c"3H"), River(c"4H", c"5H", c"6H", c"9C", c"13D"))
    assertEquals(hand, StraightFlush(6))
  }

  test("test wheel straight (A-2-3-4-5)") {
    val hand = HandComputer.findBestHand((c"1S", c"2S"), River(c"3H", c"4D", c"5C", c"7H", c"9H"))
    assertEquals(hand, Straight(5))
  }

  test("test high straight flush (10-J-Q-K-A)") {
    val hand = HandComputer.findBestHand((c"10H", c"11H"), River(c"12H", c"13H", c"1H", c"2C", c"3S"))
    assertEquals(hand, StraightFlush(1))
  }

  test("test full house beats flush") {
    val hand = HandComputer.findBestHand((c"2S", c"2D"), River(c"2C", c"5H", c"5C", c"10H", c"13H"))
    assertEquals(hand, FullHouse(2, 5))
  }

  test("test flush beats straight") {
    val hand = HandComputer.findBestHand((c"2H", c"4H"), River(c"6H", c"8H", c"10H", c"3S", c"5C"))
    assertEquals(hand, Flush(10, 8, 6, 4, 2))
  }

  test("test triple + pair = full house") {
    val hand = HandComputer.findBestHand((c"7D", c"7C"), River(c"7H", c"2D", c"2C", c"4S", c"5S"))
    assertEquals(hand, FullHouse(7, 2))
  }

  test("pair with higher kicker wins") {
    val hand = HandComputer.findBestHand((c"9S", c"9D"), River(c"2C", c"5H", c"10S", c"6D", c"3C"))
    assertEquals(hand, Pair(9, 10, 6, 5))
  }

  test("two pair from hole and river") {
    val hand = HandComputer.findBestHand((c"5S", c"11D"), River(c"5D", c"11S", c"3C", c"2H", c"7D"))
    assertEquals(hand, TwoPair(11, 5, 7))
  }

  test("three pair - pick top two") {
    val hand = HandComputer.findBestHand((c"5S", c"3D"), River(c"5H", c"3S", c"2C", c"2D", c"10C"))
    assertEquals(hand, TwoPair(5, 3, 10))
  }

  test("trip with higher kicker wins") {
    val hand = HandComputer.findBestHand((c"6S", c"6D"), River(c"6H", c"10C", c"9D", c"2H", c"3S"))
    assertEquals(hand, ThreeOfAKind(6, 10, 9))
  }

  test("two full house options - use highest trip") {
    val hand = HandComputer.findBestHand((c"4S", c"4D"), River(c"4H", c"3C", c"3S", c"2H", c"2D"))
    assertEquals(hand, FullHouse(4, 3))
  }

  test("two full house options - use highest pair when trips tie") {
    val hand = HandComputer.findBestHand((c"3S", c"3D"), River(c"3H", c"2S", c"2C", c"4C", c"4H"))
    assertEquals(hand, FullHouse(3, 4)) // trips of 3, higher pair is 4
  }

  test("four of a kind with kicker") {
    val hand = HandComputer.findBestHand((c"10S", c"10H"), River(c"10D", c"10C", c"2D", c"5S", c"6S"))
    assertEquals(hand, FourOfAKind(10, 6))
  }

  test("straight from middle of 7 cards") {
    val hand = HandComputer.findBestHand((c"2D", c"6C"), River(c"3H", c"4S", c"5D", c"8S", c"9C"))
    assertEquals(hand, Straight(6))
  }

  test("straight from multiple overlapping sequences") {
    val hand = HandComputer.findBestHand((c"5H", c"6H"), River(c"4D", c"3C", c"2C", c"7S", c"8H"))
    assertEquals(hand, Straight(8))
  }

  test("flush with high cards") {
    val hand = HandComputer.findBestHand((c"2S", c"7S"), River(c"4S", c"10S", c"12S", c"13H", c"3C"))
    assertEquals(hand, Flush(12, 10, 7, 4, 2))
  }

  test("straight flush from midcards") {
    val hand = HandComputer.findBestHand((c"5C", c"6C"), River(c"4C", c"3C", c"7C", c"9D", c"2S"))
    assertEquals(hand, StraightFlush(7))
  }

  test("straight flush - ace low") {
    val hand = HandComputer.findBestHand((c"1S", c"2S"), River(c"3S", c"4S", c"5S", c"10C", c"13H"))
    assertEquals(hand, StraightFlush(5))
  }

  test("straight flush - ace high") {
    val hand = HandComputer.findBestHand((c"10H", c"11H"), River(c"12H", c"13H", c"1H", c"2C", c"3D"))
    assertEquals(hand, StraightFlush(1)) // 1 = Ace
  }

  test("straight with kickers not interfering") {
    val hand = HandComputer.findBestHand((c"9H", c"10C"), River(c"11D", c"12C", c"13S", c"2D", c"4H"))
    assertEquals(hand, Straight(13))
  }

  test("low full house over pair and trips") {
    val hand = HandComputer.findBestHand((c"2D", c"3D"), River(c"2H", c"3H", c"3S", c"4C", c"5H"))
    assertEquals(hand, FullHouse(3, 2))
  }

  test("choose highest kicker in high card") {
    val hand = HandComputer.findBestHand((c"2C", c"4D"), River(c"7S", c"9H", c"11C", c"5H", c"3C"))
    assertEquals(hand, HighCard(11, 9, 7, 5, 4))
  }

  test("ace high flush") {
    val hand = HandComputer.findBestHand((c"1S", c"9S"), River(c"5S", c"10S", c"3S", c"7D", c"8H"))
    assertEquals(hand, Flush(1, 10, 9, 5, 3))
  }

  test("flush from board") {
    val hand = HandComputer.findBestHand((c"2D", c"3D"), River(c"5D", c"9D", c"11D", c"10D", c"12D"))
    assertEquals(hand, Flush(12, 11, 10, 9, 5))
  }

  test("straight from board") {
    val hand = HandComputer.findBestHand((c"2D", c"3D"), River(c"4H", c"5S", c"6C", c"9H", c"13H"))
    assertEquals(hand, Straight(6))
  }

  test("full house from board") {
    val hand = HandComputer.findBestHand((c"2D", c"3D"), River(c"4H", c"4D", c"4C", c"3H", c"3S"))
    assertEquals(hand, FullHouse(4, 3))
  }

  test("quads from board") {
    val hand = HandComputer.findBestHand((c"2D", c"3D"), River(c"5H", c"5D", c"5C", c"5S", c"10H"))
    assertEquals(hand, FourOfAKind(5, 10))
  }

  test("straight flush from board") {
    val hand = HandComputer.findBestHand((c"2S", c"3S"), River(c"4S", c"5S", c"6S", c"8H", c"9D"))
    assertEquals(hand, StraightFlush(6))
  }
}
