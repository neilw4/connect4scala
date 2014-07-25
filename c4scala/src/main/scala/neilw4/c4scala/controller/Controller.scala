package neilw4.c4scala.controller

import neilw4.c4scala.ui.UiCallback
import neilw4.c4scala.state._
import neilw4.c4scala.util.SimpleAsyncTask

object Controller {
  def TAG = this.getClass.toString
}

class Controller(mState: State) extends UiCallback {
    def onColumnSelected(col: Int) = {
        mState.board.add(col)
        new AsyncAiMove(mState.board).execute()
    }

    class AsyncAiMove(board: Board) extends SimpleAsyncTask[Int] {
        val evaluator: BoardController = new BoardController(board)

        override def doInBackground(): Int = evaluator.adviseMove(3)

        override def onPostExecute(col: Int) = {
            board.add(col)
        }
    }
}
