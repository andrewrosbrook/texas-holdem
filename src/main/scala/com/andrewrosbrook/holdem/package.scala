package com.andrewrosbrook

import com.andrewrosbrook.holdem.deck.Card
import com.andrewrosbrook.holdem.player.Player

package object holdem {

  /**
    * Represents an action a player can take during a betting round
    */
  sealed abstract class Action()
  case class Bet(amt: Long) extends Action
  case class Call() extends Action
  case class Check() extends Action
  case class Fold() extends Action
  case class Raise(amt: Long) extends Action

  /**
    * Represents a players hole cards
    *
    * @param first the first card
    * @param second the second card
    */
  case class HoleCards(first: Card, second: Card) {
    def toList = List(first, second)

    override def toString = {
      first.toString + " " + second.toString
    }
  }

  /**
    * Represents the five-card texas holdem board
    */
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

  val EMPTY_POT = Map[Player, Long]()
  val EMPTY_STACKS = Map[Player, Long]()
  val EMPTY_HANDS = Map[Player, HoleCards]()

  case class CurrentBet(player: Player, amt: Long)

  case class RoundState(pot: Map[Player, Long] = EMPTY_POT,
                        stacks: Map[Player, Long] = EMPTY_STACKS,
                        hands: Map[Player, HoleCards] = EMPTY_HANDS,
                        players: List[Player],
                        board: Board = Board(None, None, None)) {

    def isNewHand = hands.isEmpty
    def isPreFlop = !hands.isEmpty && !board.hasFlop()
    def isPostFlop = !hands.isEmpty && board.hasFlop() && !board.hasTurn()
    def isPostTurn = !hands.isEmpty && board.hasFlop() && board.hasTurn() && !board.hasRiver()
    def isPostRiver = !hands.isEmpty && board.hasFlop() && board.hasTurn() && board.hasRiver()

  }
}