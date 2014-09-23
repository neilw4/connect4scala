package neilw4.c4scala.controller

import neilw4.c4scala.ui.UiCallback
import neilw4.c4scala.state._
import neilw4.c4scala.util.SimpleAsyncTask

trait MoveCallback {
    def makeMove(lastCol: Int)
}

class Controller(state: State) extends UiCallback with MoveCallback {

    def onColumnSelected(col: Int) = if (state.aiThinking.isEmpty) makeMove(col)

    override def makeMove(col: Int) = {
        if (state.board.winner.isEmpty) {
            state.board.add(col)
            state.board.setWinner(checkWin(col))
            if (state.board.winner.isEmpty && state.playerAi(state.board.nextPiece)) {
                new AsyncAiMove(state, new ScalaAi(state.board), this).execute(state.difficulty)
            }
        }
    }

    def checkWin(lastCol: Int) = new ScalaAi(state.board).checkWin(lastCol)
}


class AsyncAiMove(state: State, ai: Ai, callback: MoveCallback) extends SimpleAsyncTask[Int, Int] {
    override def onPreExecute() = state.startedThinking(state.board.nextPiece)
    override def doInBackground(depth: Int): Int = ai.adviseMove(depth)
    override def onPostExecute(col: Int) = {
        state.stoppedThinking()
        callback.makeMove(col)
    }
}
