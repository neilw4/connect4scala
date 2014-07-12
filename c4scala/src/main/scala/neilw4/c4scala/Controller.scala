package neilw4.c4scala

object Controller {
  def TAG = this.getClass.toString
}

class Controller(mState: State) extends UiCallback {
    def onColumnSelected(col: Int) = {
        mState.board.add(col)
        mState.board.add(new BoardEvaluator(mState.board).adviseMove(3))
    }
}
