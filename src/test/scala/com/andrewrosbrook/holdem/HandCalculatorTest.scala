package com.andrewrosbrook.holdem

import org.junit.Test
import org.junit.Assert

class HandCalculatorTest {

  @Test
  def testPair(): Unit = {

    val pair = (
      Hand(Card(Clubs, 5), Card(Hearts, 7)),
      Board(first = Some(Card(Spades, 2)), second = Some(Card(Spades, 6)), third = Some(Card(Hearts, 5)))
    )

    val result = HandCalculator.possibleHands(pair._1, pair._2)
    Assert.assertEquals(1, result.size)
    Assert.assertEquals(Pair(Card(Clubs, 5), Card(Hearts, 5)), result(0))
  }

  @Test
  def testTwoPair(): Unit = {
    // TODO

    val twoPair = (
      Hand(Card(Clubs, 5), Card(Hearts, 7)),
      Board(first = Some(Card(Spades, 2)), second = Some(Card(Spades, 7)), third = Some(Card(Hearts, 5)))
    )
  }

  @Test
  def testThreeOfAKind(): Unit = {
    // TODO

    val threeOfAKind = (
      Hand(Card(Clubs, 5), Card(Hearts, 7)),
      Board(first = Some(Card(Spades, 5)), second = Some(Card(Spades, 6)), third = Some(Card(Hearts, 5)))
    )
  }

  @Test
  def testFourOfAKind(): Unit = {
    // TODO

    val fourOfAKind = (
      Hand(Card(Clubs, 5), Card(Hearts, 7)),
      Board(first = Some(Card(Spades, 5)), second = Some(Card(Diamonds, 5)), third = Some(Card(Hearts, 5)))
    )
  }
}
