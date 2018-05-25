package com.andrewrosbrook.holdem.rank

import com.andrewrosbrook.holdem.{Board, HoleCards}
import com.andrewrosbrook.holdem.deck.{Card, FaceValue}
import com.andrewrosbrook.holdem.deck.ordering.ByFaceValue
import com.andrewrosbrook.holdem.rank.HandRank.HandRank

object HandCalculator {

  /**
    * Calculates the best hand given hole cards and a board.
    *
    * @param hole the hole cards
    * @param board the board
    * @return the best hand
    */
  def calculate(hole: HoleCards, board: Board): Hand = {
    val highCard = hole.toList.sorted(ByFaceValue).last
    val cards = List(hole.first, hole.second) ::: board.toList()
    toHand(cards, highCard)
  }

  private def toHand(cards: List[Card], highCard: Card): Hand = {
    List(
      toStraight(cards),
      toFourOfAKind(cards),
      toFullHouse(cards),
      toFlush(cards),
      toThreeOfAKind(cards),
      toTwoPair(cards),
      toPair(cards),
      Some(Hand(HandRank.HighCard, List(highCard)))
    ).filter(_.isDefined).map(_.get).sorted(HandComparator).last
  }

  private def toStraight(cards: List[Card]): Option[Hand] = {
    val combinations = cards.combinations(HAND_SIZE)
    val straights = combinations.map(hand => {
      val sorted = hand.sorted(ByFaceValue)
      val valuesOnly = sorted.map(_.value)
      val asValueSet = (FaceValue.ValueSet.newBuilder ++= valuesOnly).result()
      if (STRAIGHTS.contains(asValueSet)) {
        val sameSuit = sorted.map(_.suit).distinct.size == 1
        val royal = sorted.last.value == FaceValue.Ace
        if (sameSuit && royal) {
          Some(Hand(HandRank.RoyalFlush, sorted))
        } else if (sameSuit) {
          Some(Hand(HandRank.StraightFlush, sorted))
        } else {
          Some(Hand(HandRank.Straight, sorted))
        }
      } else {
        None
      }
    }).filter(hand => hand.isDefined).map(hand => hand.get).toList
    straights.sorted(HandComparator).lastOption
  }

  private def toFlush(cards: List[Card]): Option[Hand] = {
    val combinations = cards.combinations(HAND_SIZE)
    val allFlushes = combinations.filter(c => isFlush(c)).map(c => c.sorted(ByFaceValue))
    if (allFlushes.isEmpty) {
      None
    } else {
      val highestFlush = allFlushes.maxBy(c => c.last.value)
      Some(Hand(HandRank.Flush, highestFlush))
    }
  }

  private def toFullHouse(cards: List[Card]): Option[Hand] = {
    toThreeOfAKind(cards) match {
      case Some(toak) => {
        val pair = toPair(cards)
        if (pair.isDefined)
          Some(Hand(HandRank.FullHouse, toak.hand ::: pair.get.hand))
        else
          None
      }
      case _ => None
    }
  }

  private def toFourOfAKind(cards: List[Card]) = toNOfAKind(cards, 4, 1, HandRank.FourOfAKind)
  private def toThreeOfAKind(cards: List[Card]) = toNOfAKind(cards, 3, 1, HandRank.ThreeOfAKind)
  private def toTwoPair(cards: List[Card]) = toNOfAKind(cards, 2, 2, HandRank.TwoPair)
  private def toPair(cards: List[Card]) = toNOfAKind(cards, 2, 1, HandRank.Pair)
  private def isFlush(cards: List[Card]) = cards.map(_.suit).distinct.size == 1

  /**
    * Find 'n' of a kind hands
    *
    * @param cards the cards to create a hand from
    * @param noSame the desired number of cards with the same value
    * @param noMatches the desired number of matching 'n' of a kind
    * @param rank rank to assign to any matching hand
    * @return the hand, if any
    */
  private def toNOfAKind(cards: List[Card], noSame: Int, noMatches: Int, rank: HandRank): Option[Hand] = {
    val expectedSize = noSame * noMatches
    val matches = cards.groupBy(_.value).filter {
      case (_, sameValueCards) => sameValueCards.size == noSame
    }.toSeq.sortBy {
      case (faceValue, _) => faceValue
    }.takeRight(noMatches).map(_._2).flatten.toSet

    if (matches.size != expectedSize) {
      None
    }
    else {
      // find kickers
      val remaining = cards.filterNot(c => matches.contains(c)).sorted(ByFaceValue)
      val kickers = remaining.takeRight(HAND_SIZE - matches.size)
      Some(Hand(rank, matches.toList, kickers))
    }
  }
}
