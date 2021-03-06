package neilw4.c4scala.controller

import android.util.Log
import neilw4.c4scala.state.{Player, Winner, StateListener, Piece}

/** Logs any changes in state for debugging. */
class LoggingListener extends StateListener {
    val TAG = "listeners"
    def log(text: String) = Log.d(TAG, text)

    override def onDifficultyChanged(difficulty: Int) = log("difficulty changed to " + difficulty)
    override def onStopAiThinking() = log("stopped thinking")
    override def onPlayerAiChanged(player: Player, isAi: Boolean) = log("player AI changed, piece=" + player + ", isAi=" + isAi)
    override def onBoardPieceChanged(x: Int, y: Int) = log("board piece changed at (" + x + ", " + y + ")")
    override def onStartAiThinking() = log("AI started thinking")
    override def onGameEnd(winner: Winner) = log("game end, winner=" + winner)
}
