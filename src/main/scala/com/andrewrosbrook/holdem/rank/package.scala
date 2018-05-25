package com.andrewrosbrook.holdem

import com.andrewrosbrook.holdem.deck.{Card, FaceValue}
import com.andrewrosbrook.holdem.rank.HandRank.HandRank

package object rank {

  val HAND_SIZE = 5
  val STRAIGHTS = FaceValue.values.sliding(HAND_SIZE).toSet

  /**
    * Defines hand rankings, from lowest to highest.
    */
  object HandRank extends Enumeration {
    type HandRank = Value
    val
      HighCard,
      Pair,
      TwoPair,
      ThreeOfAKind,
      Straight,
      Flush,
      FullHouse,
      FourOfAKind,
      StraightFlush,
      RoyalFlush = Value
  }

  /**
    * Represents a hand. The hand cards and kicker cards make up
    * the entire hand of size @{HAND_SIZE}.
    *
    * @param rank the hand rank
    * @param hand the cards pertaining to the rank
    * @param kickers kickers
    */
  case class Hand(rank: HandRank, hand: List[Card], kickers: List[Card] = List()) extends Ordered[Hand] {
    val cards = hand ::: kickers
    override def compare(that: Hand): Int = HandComparator.compare(this, that)
  }

  /**
    * Compares Hands.
    */
  object HandComparator extends Ordering[Hand] {
    override def compare(x: Hand, y: Hand): Int = {
      val rankCompare = x.rank.compare(y.rank)
      if (rankCompare == 0) {
        // calculate highest of same hand
        val xTotal = x.cards.map(_.value.id).sum
        val yTotal = y.cards.map(_.value.id).sum
        xTotal.compare(yTotal)
      } else {
        rankCompare
      }
    }
  }
}
