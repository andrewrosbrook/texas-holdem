package com.andrewrosbrook.holdem.deck

import scala.collection.mutable.ListBuffer

trait Deck {
  def shuffle(): Deck
  def deal(): Card
  def dealAll(): List[Card]
}

/**
  * Mutable representation for a deck of cards.
  *
  * A mutable deck more closely represents a real world deck of cards.
  */
class MutableDeck extends Deck {

  val cards = ListBuffer({
    suits.map(suit => {
      FaceValue.values.map(Card(suit, _))
    }).flatten.toList
  }: _*)

  // tracks deal position
  var position = 0

  override def shuffle(): Deck = {
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

  override def deal(): Card = {
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