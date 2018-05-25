package com.andrewrosbrook.holdem

import com.andrewrosbrook.holdem.deck.FaceValue.FaceValue

package object deck {

  /**
    * The face value of a card
    */
  object FaceValue extends Enumeration {
    type FaceValue = Value
    val Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace = Value
  }

  /**
    * The suit of a card
    */
  sealed abstract class Suit() extends Ordered[Suit] {
    def name = getClass.getSimpleName
    override def compare(other: Suit): Int = name.compare(other.name)
  }
  case object Hearts extends Suit()
  case object Spades extends Suit()
  case object Diamonds extends Suit()
  case object Clubs extends Suit()
  val suits = Seq(Clubs, Diamonds, Hearts, Spades)

  /**
    * A playing card
    */
  case class Card(suit: Suit, value: FaceValue) extends Ordered[Card] {
    override def toString = suit.toString + " " + value

    override def equals(that: Any): Boolean = {
      that match {
        case that: Card => compare(that) == 0
        case _ => false
      }
    }

    override def compare(other: Card): Int = {
      val suitCompare = suit.name.compare(other.suit.name)
      if (suitCompare == 0) {
        value.compare(other.value)
      } else {
        suitCompare
      }
    }
  }

  /**
    * Deck orderings
    */
  object ordering {
    object ByFaceValue extends Ordering[Card] {
      override def compare(x: Card, y: Card): Int = x.value.compare(y.value)
    }
  }
}
