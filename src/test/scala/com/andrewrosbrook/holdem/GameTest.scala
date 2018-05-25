package com.andrewrosbrook.holdem

import com.andrewrosbrook.holdem.deck.{Card, Deck}
import com.andrewrosbrook.holdem.player.Player
import org.junit.{Assert, Test}

import scala.collection.mutable

class GameTest {

  val numRounds = 1
  val startingAmount = 1000

  @Test
  def testPreFlopWinner(): Unit = {

    val players = nMockPlayers(3)

    players(0).addAction(Bet(50))
    players(1).addAction(Raise(100))
    players(2).addAction(Fold())
    players(0).addAction(Fold())

    val endState = Game.start(numRounds, players, startingAmount)
    Assert.assertEquals(950, endState.stacks(players(0)))
    Assert.assertEquals(1050, endState.stacks(players(1)))
    Assert.assertEquals(1000, endState.stacks(players(2)))
  }

  @Test
  def testPreTurnWinner(): Unit = {

    val players = nMockPlayers(3)

    // pre flop
    players(0).addAction( Bet(100))
    players(1).addAction(Call())
    players(2).addAction(Call())

    // post flop
    players(0).addAction(Check())
    players(1).addAction(Bet(100))
    players(2).addAction(Fold())
    players(0).addAction(Fold())

    val endState = Game.start(numRounds, players, startingAmount)
    Assert.assertEquals(900, endState.stacks(players(0)))
    Assert.assertEquals(1200, endState.stacks(players(1)))
    Assert.assertEquals(900, endState.stacks(players(2)))
  }

  @Test
  def testPreRiverWinner(): Unit = {

    val players = nMockPlayers(3)

    // pre flop
    players(0).addAction(Bet(100))
    players(1).addAction(Call())
    players(2).addAction(Call())

    // post flop
    players(0).addAction(Check())
    players(1).addAction(Bet(100))
    players(2).addAction(Call())
    players(0).addAction(Call())

    // post turn
    players(0).addAction(Check())
    players(1).addAction(Check())
    players(2).addAction(Bet(100))
    players(0).addAction(Call())
    players(1).addAction(Raise(200))
    players(2).addAction(Fold())
    players(0).addAction(Fold())

    val endState = Game.start(numRounds, players, startingAmount)
    Assert.assertEquals(800, endState.stacks(players(0)))
    Assert.assertEquals(1500, endState.stacks(players(1)))
    Assert.assertEquals(700, endState.stacks(players(2)))
  }

  @Test
  def testPreShowdownWinner(): Unit = {

    val players = nMockPlayers(3)

    // pre flop
    players(0).addAction(Bet(100))
    players(1).addAction(Call())
    players(2).addAction(Call())

    // post flop
    players(0).addAction(Bet(100))
    players(1).addAction(Call())
    players(2).addAction(Call())

    // post turn
    players(0).addAction(Bet(100))
    players(1).addAction(Call())
    players(2).addAction(Call())

    // post river
    players(0).addAction(Bet(100))
    players(1).addAction(Fold())
    players(2).addAction(Fold())

    val endState = Game.start(numRounds, players, startingAmount)
    Assert.assertEquals(1600, endState.stacks(players(0)))
    Assert.assertEquals(700, endState.stacks(players(1)))
    Assert.assertEquals(700, endState.stacks(players(2)))
  }

  private def nMockPlayers(n: Int) = {
    (0 until n).map(x => ProgrammablePlayer("player_" + x)).toList
  }

  case class ProgrammablePlayer(name: String) extends Player {

    val actions = mutable.ListBuffer[Action]()
    val capture = mutable.ListBuffer[ArgumentCapture]()

    def addAction(action: Action) = actions += action

    override def act(board: Board,
                     currentBet: Option[CurrentBet],
                     holeCards: HoleCards,
                     yourStack: Long,
                     yourBet: Long): Action = {

      capture += ArgumentCapture(board, currentBet, holeCards, yourStack, yourBet)
      print(s"${name} to act: ")
      val action = actions.remove(0)
      println(s"${action}")
      action
    }

    case class ArgumentCapture(board: Board,
                               currentBet: Option[CurrentBet],
                               holeCards: HoleCards,
                               yourStack: Long,
                               yourBet: Long) {}
  }

  class ProgrammableDeck() extends Deck {

    val cards = mutable.ListBuffer[Card]()

    override def shuffle(): Deck = this
    override def deal(): deck.Card = cards.remove(0)
    override def dealAll() = cards.toList
  }
}
