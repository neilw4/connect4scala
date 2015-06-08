package neilw4.c4scala.controller

import neilw4.c4scala.state._

/** Controls the internal logic of the application. */
class Controller(state: State) extends StateListener {
    state.attachListener(this)
    val loggingListener = new LoggingListener
    state.attachListener(loggingListener)

    // The AI task being run if it exists.
    var asyncAi: Option[AsyncAi] = None

    def makeMove(col: Int) = {
        state.board.stopAiThinking()
        if (state.board.winner.isEmpty) {
            state.board.add(col)
            state.board.checkWinner(col)
        }
    }

    def onDestroy() = {
        state.removeListener(this)
        state.removeListener(loggingListener)
    }

    /** If the difficulty changes while the AI is playing, restart the AI. */
    override def onDifficultyChanged(difficulty: Int) = if (state.board.aiThinking) state.board.startAiThinking()

    /** If the current player has changed between human and AI, stop/start the AI. */
    override def onPlayerAiChanged(player: Player, isAi: Boolean) = (player == state.board.nextPlayer, isAi) match {
        case (true, true) => state.board.startAiThinking()
        case (true, false) => state.board.stopAiThinking()
        case _ =>
    }

    /** When a move is made, check if the AI should play. */
    override def onBoardPieceChanged(x: Int, y: Int) = if (state.playerAi(state.board.nextPlayer)) state.board.startAiThinking()

    /** Starts an AsyncTask for the AI to run. */
    override def onStartAiThinking() = {
        if (state.board.winner.isEmpty && state.playerAi(state.board.nextPlayer)) {
            val task = new ScalaAi(state.board, this)
            asyncAi = Some(task)
            task.execute(state.difficulty)
        }
    }

    /** Stops the AsyncTask the AI is using. */
    override def onStopAiThinking() = {
        asyncAi.foreach(_.cancel(true))
        asyncAi = None
    }
    override def onGameEnd(winner: Winner) = state.board.stopAiThinking()
}
