package com.andrewrosbrook.holdem

class Game {

  def start(numRounds: Int, players: List[Player], startingAmount: Long) = {

    val deck = new MutableDeck()
    val rounds = 1 to numRounds
    val initialStacks = players.map((_, startingAmount)).toMap
    val initialState = new RoundState(players = players, stacks = initialStacks)
    rounds.foldLeft(initialState)((state, roundNo) => {

      println(s"Starting round no. $roundNo")
      val roundEndState = round(deck, state)
      roundEndState.copy(
        pot = EMPTY_POT,
        hands = EMPTY_HANDS,
        players = players,
        board = Board(None, None, None)
      )
    })
  }

  private def round(deck: MutableDeck, state: RoundState, totalPot: Long = 0): RoundState = {

    val newState = if (state.isNewHand) {
      val hands = dealHands(state.players, deck)
      bettingRound(state.copy(hands = hands), None)
    } else if (state.isPreFlop) {
      val flop = dealFlop(deck, state.board)
      bettingRound(state.copy(board = flop), None)
    } else if (state.isPostFlop) {
      val turn = dealTurn(deck, state.board)
      bettingRound(state.copy(board = turn), None)
    } else if (state.isPostTurn) {
      val river = dealRiver(deck, state.board)
      bettingRound(state.copy(board = river), None)
    } else {
      // showdown
      state
    }

    val newTotalPot = totalPot + newState.pot.values.sum

    if (newState.players.size == 1) {
      val winner = newState.players(0)
      println(s"${winner.name} wins the round")
      addToStack(newState, winner, newTotalPot)
    } else if (newState.isPostRiver) {
      val winner = showdown(newState)
      println(s"${winner.name} wins the round after showdown")
      addToStack(newState, winner, newTotalPot)
    } else {
      round(deck, emptyPot(newState), newTotalPot)
    }
  }

  private def bettingRound(state: RoundState, initialBet: Option[CurrentBet]): RoundState = {
    case class BettingState(roundState: RoundState, currentBet: Option[CurrentBet])

    val initialState = BettingState(state, initialBet)
    val newBettingState = state.players.foldLeft(initialState)((state, p) => {

      val roundState = state.roundState
      val bet = state.currentBet

      if (bet.isDefined && bet.get.player == p) {
        // the player with the current highest bet does not act
        // again within the betting round unless another player
        // raises, hence we return the state with no changes
        // and move onto the next player
        state
      } else {

        val playerHand = roundState.hands.get(p).get
        val playerStack = roundState.stacks.get(p).get
        val playerBet = roundState.pot.getOrElse(p, 0l)
        val action = p.act(roundState.board, bet, playerHand, playerStack, playerBet)
        val newState = action match {
          case Bet(amt) => onBet(roundState, p, amt)
          case Call() => onCall(roundState, p, bet.get.amt)
          case Check() => roundState
          case Fold() => onFold(roundState, p)
          case Raise(amt) => onBet(roundState, p, amt)
        }
        val newCurrentBet = action match {
          case Bet(amt) => Some(CurrentBet(p, amt))
          case Raise(amt) => Some(CurrentBet(p, amt))
          case _ => bet
        }

        BettingState(newState, newCurrentBet)
      }
    })


    if (isBettingRoundComplete(newBettingState.roundState)) {
      newBettingState.roundState
    } else {
      bettingRound(newBettingState.roundState, newBettingState.currentBet)
    }
  }

  private def onBet(state: RoundState, player: Player, amt: Long) = {
    // TODO validate remaining chips / "all in" scenario
    addToPot(state, player, amt)
  }

  private def onCall(state: RoundState, player: Player, calledBet: Long) = {
    val delta = calledBet - state.pot.getOrElse(player, 0l)
    addToPot(state, player, delta)
  }

  private def onFold(state: RoundState, player: Player) = {
    val newPlayers = state.players.filterNot(_ == player)
    state.copy(players = newPlayers)
  }

  private def addToPot(state: RoundState, player: Player, amt: Long) = {
    val newStackAmt = state.stacks.getOrElse(player, 0l) - amt
    val newPotAmt = state.pot.getOrElse(player, 0l) + amt

    val newStacks = state.stacks.updated(player, newStackAmt)
    val newPot = state.pot.updated(player, newPotAmt)
    state.copy(pot = newPot, stacks = newStacks)
  }

  private def addToStack(state: RoundState, player: Player, amt: Long) = {
    val newStackAmt = state.stacks.getOrElse(player, 0l) + amt
    val newStacks = state.stacks.updated(player, newStackAmt)
    state.copy(stacks = newStacks)
  }

  private def emptyPot(state: RoundState) = {
    state.copy(pot = EMPTY_POT)
  }

  private def showdown(state: RoundState): Player = {
    // TODO
    null
  }

  private def isBettingRoundComplete(state: RoundState) = {
    val distinctValues = state.players.map(p => state.pot.getOrElse(p, 0l)).distinct

    // each remaining player should have contributed the same amount to the pot
    // if not - the betting round is not complete
    distinctValues.size == 1
  }

  private def isRoundComplete(state: RoundState) = {
    state.players == 1 || state.isPostRiver
  }

  private def dealHands(players: List[Player], deck: MutableDeck) = {
    players.map(
      // deal the first card
      p => (p, deck.deal())
    ).map(
      // deal the second card
      p => (p._1, Hand(p._2, deck.deal()))
    ).toMap
  }

  private def burnCard(deck: MutableDeck) = deck.deal()

  private def dealFlop(deck: MutableDeck, board: Board) = {
    burnCard(deck)
    val flop = (
      deck.deal(),
      deck.deal(),
      deck.deal()
    )
    board.copy(flop = Some(flop))
  }

  private def dealTurn(deck: MutableDeck, board: Board) = {
    burnCard(deck)
    board.copy(turn = Some(deck.deal()))
  }

  private def dealRiver(deck: MutableDeck, board: Board) = {
    burnCard(deck)
    board.copy(turn = Some(deck.deal()))
  }
}
