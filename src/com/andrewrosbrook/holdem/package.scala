package com.andrewrosbrook

package object holdem {

  abstract class Action()
  case class Bet(amt: Long) extends Action
  case class Call() extends Action
  case class Check() extends Action
  case class Fold() extends Action
  case class Raise(amt: Long) extends Action

  abstract class Suit()
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

  case class Board(flop: Option[Flop], turn: Option[Turn], river: Option[River]) {
    override def toString = {
      val blank = ""
      val space = " "
      flop.getOrElse(blank) + space + turn.getOrElse(blank) + space + river.getOrElse(blank)
    }
  }

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
    def isPreFlop = !hands.isEmpty && !board.flop.isDefined
    def isPostFlop = !hands.isEmpty && board.flop.isDefined && !board.turn.isDefined
    def isPostTurn = !hands.isEmpty && board.flop.isDefined && board.turn.isDefined && !board.river.isDefined
    def isPostRiver = !hands.isEmpty && board.flop.isDefined && board.turn.isDefined && board.river.isDefined

  }
}
