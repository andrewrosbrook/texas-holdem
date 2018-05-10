package com.andrewrosbrook.holdem

object HandCalculator {

  def possibleHands(hand: Hand, board: Board) = {
    // TODO straights and flushes
    val cards = List(hand.first, hand.second) ::: board.toList()
    pairs(cards)
  }

  private def pairs(cards: List[Card]): List[HandType] = {
    cards.map(
      // find all occurences for the value of this card
      c => cards.filter(c.values == _.values)
    ).filter(
      // filter out any non paired values
      _.size > 1
    ).map(pair => {
      // create domain object
      pair.size match {
        case 2 => Pair(pair(0), pair(1))
        case 3 => ThreeOfAKind(pair(0), pair(1), pair(2))
        case 4 => FourOfAKind(pair(0), pair(1), pair(2), pair(3))
      }
    }).distinct
  }
}
