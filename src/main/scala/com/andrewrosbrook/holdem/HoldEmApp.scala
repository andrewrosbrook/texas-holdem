package com.andrewrosbrook.holdem

import com.andrewrosbrook.holdem.player.{ConsolePlayer, Player}

object HoldEmApp extends App {

  val players = List(new ConsolePlayer("Lisa"), new ConsolePlayer("Andy"), new ConsolePlayer("Phil"))
  val initialStack = 1000l
  val numRounds = 5
  Game.start(numRounds, players, initialStack)
}
