package com.andrewrosbrook.holdem.deck

import com.andrewrosbrook.holdem.deck.FaceValue._
import org.junit.{Assert, Test}

class MutableDeckTest {

  @Test
  def testDeal(): Unit = {
    val deck = new MutableDeck()
    Assert.assertEquals(Card(Clubs, Two), deck.deal())
  }

  @Test
  def testDealAll(): Unit = {
    val deck = new MutableDeck()
    val allCards = deck.dealAll()
    Assert.assertEquals(52, allCards.size)
  }

  @Test
  def testShuffle(): Unit = {
    // test that the first 13 cards are not all clubs, as per
    // a new unshuffled deck.
    //
    // TODO a more deterministic way to test this?
    val deck = new MutableDeck().shuffle()
    val distinctSuits = deck.dealAll().take(13).map(_.suit).distinct
    val clubsOnly = List(Clubs)
    Assert.assertNotEquals(clubsOnly, distinctSuits)
  }
}

