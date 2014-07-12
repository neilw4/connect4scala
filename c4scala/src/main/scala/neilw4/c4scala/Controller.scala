package neilw4.c4scala

import android.os.AsyncTask

object Controller {
  def TAG = this.getClass.toString
}

class Controller(mState: State) extends UiCallback {
    def onColumnSelected(col: Int) = {
        mState.board.add(col)
        val evaluator: BoardEvaluator = new BoardEvaluator(mState.board)
        new AsyncAiMove(mState.board).execute()
    }

    class AsyncAiMove(board: Board) extends AsyncTask[AnyRef, AnyRef, Int] {
        val evaluator: BoardEvaluator = new BoardEvaluator(board)

        override def doInBackground(params: AnyRef*): Int = evaluator.adviseMove(3)

        override def onPostExecute(col: Int) = board.add(col)
    }
}
