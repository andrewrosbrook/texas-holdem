package com.andrewrosbrook

package object holdem {

  sealed abstract class Action()
  case class Bet(amt: Long) extends Action
  case class Call() extends Action
  case class Check() extends Action
  case class Fold() extends Action
  case class Raise(amt: Long) extends Action

  sealed abstract class Suit()
  case object Hearts extends Suit
  case object Spades extends Suit
  case object Diamonds extends Suit
  case object Clubs extends Suit

  case class Card(suit: Suit, values: Int*) {
    override def toString = suit.toString + " " + values.mkString(",")
  }

  type Flop = (Card, Card, Card)
  type Turn = Card
  type River = Card

  case class Hand(first: Card, second: Card) {
    override def toString = {
      first.toString + " " + second.toString
    }
  }

  case class Board(first: Option[Card] = None,
                   second: Option[Card] = None,
                   third: Option[Card] = None,
                   fourth: Option[Card] = None,
                   fifth: Option[Card] = None) {

    def hasFlop() = first.isDefined && second.isDefined && third.isDefined
    def hasTurn() = hasFlop() && fourth.isDefined
    def hasRiver() = hasFlop() && hasTurn() && fifth.isDefined
    def toList() = {
      List(first, second, third, fourth, fifth).filter(c => c.isDefined).map(_.get)
    }

    override def toString = toList.mkString(" ")
  }

  sealed abstract class HandType
  case class Pair(first: Card, second: Card) extends HandType
  case class TwoPair(firstPair: Pair, secondPair: Pair) extends HandType
  case class ThreeOfAKind(first: Card, second: Card, third: Card) extends HandType
  case class Straight(first: Card, second: Card, third: Card, fourth: Card, fifth: Card) extends HandType
  case class Flush(first: Card, second: Card, third: Card, fourth: Card, fifth: Card) extends HandType
  case class FullHouse(pair: Pair, threeOfAKind: ThreeOfAKind) extends HandType
  case class FourOfAKind(first: Card, second: Card, third: Card, fourth: Card) extends HandType
  case class StraightFlush(first: Card, second: Card, third: Card, fourth: Card, fifth: Card) extends HandType
  case class RoyalFlush(first: Card, second: Card, third: Card, fourth: Card, fifth: Card) extends HandType

  case class CurrentBet(player: Player, amt: Long)

  val EMPTY_POT = Map[Player, Long]()
  val EMPTY_STACKS = Map[Player, Long]()
  val EMPTY_HANDS = Map[Player, Hand]()

  case class RoundState(pot: Map[Player, Long] = EMPTY_POT,
                        stacks: Map[Player, Long] = EMPTY_STACKS,
                        hands: Map[Player, Hand] = EMPTY_HANDS,
                        players: List[Player],
                        board: Board = Board(None, None, None)) {

    def isNewHand = hands.isEmpty
    def isPreFlop = !hands.isEmpty && !board.hasFlop()
    def isPostFlop = !hands.isEmpty && board.hasFlop() && !board.hasTurn()
    def isPostTurn = !hands.isEmpty && board.hasFlop() && board.hasTurn() && !board.hasRiver()
    def isPostRiver = !hands.isEmpty && board.hasFlop() && board.hasTurn() && board.hasRiver()

  }
}