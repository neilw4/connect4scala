package neilw4.c4scala.controller

import neilw4.c4scala.ui.UiCallback
import neilw4.c4scala.state._
import neilw4.c4scala.util.SimpleAsyncTask

trait MoveCallback {
    def makeMove(lastCol: Int)
}

class Controller(state: State) extends UiCallback with MoveCallback {

    def onColumnSelected(col: Int) = if (!state.thinking) makeMove(col)

    override def makeMove(col: Int) = {
        if (state.board.winner.isEmpty) {
            state.board.add(col)
            state.setThinking(false)
            state.board.setWinner(checkWin(col))
            if (state.board.winner.isEmpty && state.playerAi(state.board.nextPiece)) {
                new AsyncAiMove(state, new ScalaAi(state.board), this).execute(state.difficulty)
            }
        }
    }

    def checkWin(lastCol: Int): Option[Piece] = new ScalaAi(state.board).checkWin(lastCol) match {
        case ScalaAi.NO_WIN => None
        case ScalaAi.DRAW => Some(BLANK)
        case ScalaAi.WIN => Some(state.board.nextPiece)
        case ScalaAi.LOSE => Some(state.board.nextPiece.opposite)
    }
}


class AsyncAiMove(state: State, ai: Ai, callback: MoveCallback) extends SimpleAsyncTask[Int, Int] {
    state.setThinking(true)
    override def doInBackground(depth: Int): Int = ai.adviseMove(depth)
    override def onPostExecute(col: Int) = callback.makeMove(col)
}
