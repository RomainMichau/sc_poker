import Common.testDeck
import Suit.{Clubs, Diamonds, Hearts, Spades}
import munit.FunSuite

class DeckTests extends FunSuite {
  test("shuffled deck contains 52 unique cards") {
    val deck = Deck.shuffled()
    assertEquals(deck.cards.size, 52)
    assertEquals(deck.cards.groupBy(c => c).size, 52)
    println(deck.cards)
  }

  test("Random generation work fine (test 10000 times)") {
    val cards = for {
      _ <- 1 to 10000
    } yield Deck.shuffled().cards
    assertEquals(cards.groupBy(c => c).size, 10000)
  }

  test("Pick a card remove a card from the deck and return the first cards") {
    val (newDeck, c1) = testDeck.pickCard()
    assertEquals(newDeck.cards.size, 51)
    assertEquals(newDeck.cards.head, Card(Hearts, 13))
    assertEquals(c1, Card(Spades, 1))
    val (newDeck2, c2) = newDeck.pickCard()
    assertEquals(newDeck2.cards.size, 50)
    assertEquals(newDeck2.cards.head, Card(Diamonds, 5))
    assertEquals(c2, Card(Hearts, 13))
    assertNotEquals(c1, c2)
  }

  test("Deal cards") {
    val (hands, deck) = testDeck.dealHands(3)
    val expectedHands = Seq((Card(Spades, 1), Card(Hearts, 13)), (Card(Diamonds, 5), Card(Diamonds, 1)), (Card(Clubs, 11),
      Card(Clubs, 10)))
    assertEquals(hands, expectedHands)
    assertEquals(deck.cards.size, 52 - 6)
  }
}

