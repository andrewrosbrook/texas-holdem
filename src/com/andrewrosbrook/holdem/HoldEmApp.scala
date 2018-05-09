package com.andrewrosbrook.holdem

object HoldEmApp extends App {

  val game = new Game()
  val players = List(new Player("a"), new Player("b"), new Player("c"))
  game.start(5, players, 1000l)
}
