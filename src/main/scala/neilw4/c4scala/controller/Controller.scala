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
        stopAiMove()
        if (state.board.winner.isEmpty) {
            state.board.add(col)
            state.board.checkWinner(col)
        }
    }

    /** Starts the AI if relevant. */
    def startAiMove() = {
        stopAiMove()
        if (state.board.winner.isEmpty && state.playerAi(state.board.nextPiece)) {
            state.board.startedThinking()
            val task = new ScalaAi(state.board, this)
            asyncAi = Some(task)
            task.execute(state.difficulty)
        }
    }

    /** Stops the AI if it is running. */
    def stopAiMove() = {
        asyncAi.foreach(_.cancel(true))
        asyncAi = None
        state.board.stoppedThinking()
    }

    def onDestroy() = {
        state.removeListener(this)
        state.removeListener(loggingListener)
    }

    /** If the difficulty changes while the AI is playing, restart the AI. */
    override def onDifficultyChanged(difficulty: Int) = if (state.board.aiThinking) startAiMove

    /** If the current player has changed between human and AI, stop/start the AI. */
    override def onPlayerAiChanged(piece: Piece, isAi: Boolean) = (piece == state.board.nextPiece, isAi) match {
        case (true, true) => startAiMove()
        case (true, false) => stopAiMove()
        case _ =>
    }

    /** When a move is made, check if the AI should play. */
    override def onBoardPieceChanged(x: Int, y: Int) = startAiMove()

    override def onStartThinking() = {}
    override def onStopThinking() = {}
    override def onGameEnd(winner: Piece) = stopAiMove()
}
