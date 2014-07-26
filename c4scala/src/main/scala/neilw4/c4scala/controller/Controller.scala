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
        state.board.add(col)
        state.thinking = false
        if (checkWin(col) != null) {
            // TODO: nicer way to end game
            state.thinking = true
        } else {
            if (state.playerAi(state.board.nextPiece)) {
                state.thinking = true
                new AsyncAiMove(new ScalaAi(state.board), this).execute(state.difficulty)
            }
        }
    }

    def checkWin(lastCol: Int): Piece = new ScalaAi(state.board).checkWin(lastCol) match {
        case ScalaAi.NO_WIN => null
        case ScalaAi.DRAW => BLANK
        case ScalaAi.WIN => state.board.nextPiece
        case ScalaAi.LOSE => state.board.nextPiece.opposite
    }
}


class AsyncAiMove(ai: Ai, callback: MoveCallback) extends SimpleAsyncTask[Int, Int] {
    override def doInBackground(depth: Int): Int = ai.adviseMove(depth)
    override def onPostExecute(col: Int) = callback.makeMove(col)
}
