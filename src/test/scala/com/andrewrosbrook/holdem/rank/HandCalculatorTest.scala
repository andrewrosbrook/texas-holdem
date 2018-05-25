package com.andrewrosbrook.holdem.rank

import com.andrewrosbrook.holdem.deck.FaceValue._
import com.andrewrosbrook.holdem.deck._
import com.andrewrosbrook.holdem.{Board, HoleCards}
import org.junit.{Assert, Test}

class HandCalculatorTest {

  @Test
  def testHighCard(): Unit = {
    val hand = newHand(Card(Clubs, Ace), Card(Hearts, King))
    val board = newBoard(Card(Spades, Two), Card(Spades, Six), Card(Hearts, Five))
    val result = HandCalculator.calculate(hand, board)
    Assert.assertEquals(HandRank.HighCard, result.rank)
    Assert.assertEquals(List(Card(Clubs, Ace)), result.hand)
  }

  @Test
  def testPair(): Unit = {
    val hand = newHand(Card(Clubs, Five), Card(Hearts, Seven))
    val board = newBoard(Card(Spades, Two), Card(Spades, Six), Card(Hearts, Five))
    val result = HandCalculator.calculate(hand, board)
    Assert.assertEquals(HandRank.Pair, result.rank)
    Assert.assertEquals(List(
      Card(Clubs, Five),
      Card(Hearts, Five)), result.hand)
    Assert.assertEquals(List(
      Card(Spades, Two),
      Card(Spades, Six),
      Card(Hearts, Seven)), result.kickers)
  }

  @Test
  def testTwoPair(): Unit = {
    val hand = newHand(Card(Clubs, Five), Card(Hearts, Seven))
    val board = newBoard(Card(Spades, Two),
      Card(Spades, Seven),
      Card(Hearts, Five),
      Card(Diamonds, Ace),
      Card(Diamonds, King)
    )
    val result = HandCalculator.calculate(hand, board)
    Assert.assertEquals(HandRank.TwoPair, result.rank)
    Assert.assertEquals(List(Card(Clubs, Five),
      Card(Hearts, Five),
      Card(Hearts, Seven),
      Card(Spades, Seven)), result.hand)
    Assert.assertEquals(List(Card(Diamonds, Ace)), result.kickers)
  }

  @Test
  def testThreeOfAKind(): Unit = {
    val hand = newHand(Card(Clubs, Five), Card(Hearts, Seven))
    val board = newBoard(
      Card(Diamonds, Seven),
      Card(Spades, Seven),
      Card(Hearts, Four)
    )
    val result = HandCalculator.calculate(hand, board)
    Assert.assertEquals(HandRank.ThreeOfAKind, result.rank)
    Assert.assertEquals(List(
      Card(Hearts, Seven),
      Card(Diamonds, Seven),
      Card(Spades, Seven)), result.hand)
    Assert.assertEquals(List(Card(Hearts, Four), Card(Clubs, Five)), result.kickers)
  }

  @Test
  def testStraight(): Unit = {
    val hand = newHand(Card(Clubs, Two), Card(Hearts, Five))
    val board = newBoard(
      Card(Spades, Four),
      Card(Spades, Six),
      Card(Hearts, Three),
      Card(Spades, Seven),
      Card(Spades, Ace)
    )
    val result = HandCalculator.calculate(hand, board)
    Assert.assertEquals(HandRank.Straight, result.rank)
    Assert.assertEquals(List(
      Card(Hearts, Three),
      Card(Spades, Four),
      Card(Hearts, Five),
      Card(Spades, Six),
      Card(Spades, Seven)), result.hand)
    Assert.assertEquals(List(), result.kickers)
  }

  @Test
  def testFullHouse(): Unit = {
    val hand = newHand(Card(Clubs, Five), Card(Hearts, Five))
    val board = newBoard(
      Card(Spades, Six),
      Card(Diamonds, Six),
      Card(Hearts, Two),
      Card(Diamonds, Two),
      Card(Spades, Two)
    )
    val result = HandCalculator.calculate(hand, board)
    Assert.assertEquals(HandRank.FullHouse, result.rank)
    Assert.assertEquals(List(
      Card(Hearts, Two),
      Card(Diamonds, Two),
      Card(Spades, Two),
      Card(Spades, Six),
      Card(Diamonds, Six)), result.hand)
    Assert.assertEquals(List(), result.kickers)
  }

  @Test
  def testFlush(): Unit = {
    val hand = newHand(Card(Clubs, Five), Card(Clubs, Eight))
    val board = newBoard(
      Card(Clubs, Nine),
      Card(Clubs, Two),
      Card(Clubs, Three),
      Card(Diamonds, Jack),
      Card(Spades, Two)
    )
    val result = HandCalculator.calculate(hand, board)
    Assert.assertEquals(HandRank.Flush, result.rank)
    Assert.assertEquals(List(
      Card(Clubs, Two),
      Card(Clubs, Three),
      Card(Clubs, Five),
      Card(Clubs, Eight),
      Card(Clubs, Nine)), result.hand)
    Assert.assertEquals(List(), result.kickers)
  }

  @Test
  def testFourOfAKind(): Unit = {
    val hand = newHand(Card(Clubs, Five), Card(Hearts, Five))
    val board = newBoard(
      Card(Spades, Six),
      Card(Diamonds, Six),
      Card(Hearts, Two),
      Card(Diamonds, Five),
      Card(Spades, Five)
    )
    val result = HandCalculator.calculate(hand, board)
    Assert.assertEquals(HandRank.FourOfAKind, result.rank)
    Assert.assertEquals(List(
      Card(Clubs, Five),
      Card(Hearts, Five),
      Card(Diamonds, Five),
      Card(Spades, Five)), result.hand)
    Assert.assertEquals(List(Card(Diamonds, Six)), result.kickers)

  }

  private def newHand(first: Card, second: Card) = HoleCards(first, second)
  private def newBoard(first: Card, second: Card, third: Card) = Board(Some(first), Some(second), Some(third))
  private def newBoard(first: Card,
                       second: Card,
                       third: Card,
                       fourth: Card,
                       fifth: Card) = Board(Some(first), Some(second), Some(third), Some(fourth), Some(fifth))
}
