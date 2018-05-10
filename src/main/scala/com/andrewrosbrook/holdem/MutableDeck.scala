package com.andrewrosbrook.holdem

import scala.collection.mutable.ListBuffer

/**
  * Mutable representation for a deck of cards.
  */
class MutableDeck {

  val cards = ListBuffer({
    val suits = Seq(Hearts, Clubs, Diamonds, Spades)
    suits.map(suit => {
      val special = List(
        new Card(suit, 11),   // jack
        new Card(suit, 12),   // queen
        new Card(suit, 13),   // king
        new Card(suit, 1, 14) // ace
      )
      2.to(10).map(new Card(suit, _)).toList ::: special
    }).flatten.toList
  }: _*)

  // tracks deal position
  var position = 0

  def shuffle(): MutableDeck = {
    val rand = scala.util.Random
    (position until cards.size).map(pos => {
      val newPos = rand.nextInt(cards.size)
      val thisCard = cards(pos)
      val switchCard = cards(newPos)

      cards(pos) = switchCard
      cards(newPos) = thisCard
    })
    this
  }

  def deal(): Card = {
    if (position == cards.size) {
      shuffleAndReset()
      deal()
    }
    val card = cards(position)
    position = position + 1
    card
  }

  def dealAll() = cards.toList

  private def shuffleAndReset(): Unit = {
    position = 0
    shuffle()
  }
}