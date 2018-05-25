package com.andrewrosbrook.holdem.player

import com.andrewrosbrook.holdem._

import scala.io.StdIn

trait Player {
  def name: String
  def act(board: Board,
          currentBet: Option[CurrentBet],
          holeCards: HoleCards,
          yourStack: Long,
          yourBet: Long): Action
}

case class ConsolePlayer(name: String) extends Player {

  override def act(board: Board,
                  currentBet: Option[CurrentBet],
                  hand: HoleCards,
                  yourStack: Long,
                  yourBet: Long): Action = {

    printState(board, hand)

    val allowedActions = currentBet match {
      case None => {
        println("\t there is no current bet")
        List(Check(), Fold(), Bet(0))
      }
      case Some(bet) => {
        if (yourBet == bet.amt) {
          println(s"\tthe current bet is ${bet.amt} by ${bet.player.name}, your bet is $yourBet")
          List(Check(), Fold(), Raise(0))
        }
        else {
          val amtToCall = bet.amt - yourBet
          println(s"\tthe current bet is ${bet.amt} by ${bet.player.name}, your bet is $yourBet. you require $amtToCall to call")
          List(Call(), Fold(), Raise(0))
        }
      }
    }

    promptForInput(allowedActions)
  }

  private def printState(board: Board, hand: HoleCards) = {
    println()
    println(s"$name it is your turn to act")
    println()
    println("\tBoard\t\t" + board.toString)
    println("\tYour Hand\t\t" + hand.toString)
    println()
  }

  private def promptForInput(allowedActions: List[Action]): Action = {

    print("\t")
    allowedActions.map(action => {
      val index = allowedActions.indexOf(action)
      print(s" ($index) ${action.getClass.getSimpleName}")
    })
    println()

    val choice = StdIn.readInt()
    if (choice >= allowedActions.size) {
      println("Unrecognized action")
      promptForInput(allowedActions)
    } else {

      val action = allowedActions(choice)

      action match {
        case Bet(0) => Bet(promptForAmount())
        case Raise(0) => Raise(promptForAmount())
        case _ => action
      }
    }
  }

  private def promptForAmount() = {
    print("\t\tamount: ")
    val result = StdIn.readLong()
    println(); result
  }
}
