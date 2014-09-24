package neilw4.c4scala.controller

import android.util.Log
import neilw4.c4scala.state.{StateListener, Piece}

/** Logs any changes in state for debugging. */
class LoggingListener extends StateListener {
    val TAG = "listeners"
    def log(text: String) = Log.d(TAG, text)

    override def onDifficultyChanged(difficulty: Int) = log("difficulty changed to " + difficulty)
    override def onStopThinking() = log("stopped thinking")
    override def onPlayerAiChanged(piece: Piece, isAi: Boolean) = log("player AI changed, piece=" + piece + ", isAi=" + isAi)
    override def onBoardPieceChanged(x: Int, y: Int) = log("board piece changed at (" + x + ", " + y + ")")
    override def onStartThinking() = log("AI started thinking")
    override def onGameEnd(winner: Piece) = log("game end, winner=" + winner)
}
