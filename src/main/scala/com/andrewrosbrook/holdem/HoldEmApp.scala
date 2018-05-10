package com.andrewrosbrook.holdem

object HoldEmApp extends App {

  val game = new Game()
  val players = List(new Player("Lisa"), new Player("Andy"), new Player("Phil"))
  val initialStack = 1000l
  val numRounds = 5
  game.start(numRounds, players, initialStack)
}
