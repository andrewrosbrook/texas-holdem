package com.andrewrosbrook.holdem

import scala.io.StdIn

case class Player(name: String) {

  def act(board: Board,
          currentBet: Option[CurrentBet],
          hand: Hand,
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

  private def printState(board: Board, hand: Hand) = {
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
