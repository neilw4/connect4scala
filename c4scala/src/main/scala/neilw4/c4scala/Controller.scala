package neilw4.c4scala

object Controller {
  def TAG = this.getClass().toString()
}

class Controller(mState: State) extends UiCallback {
    def onColumnSelected(column: Int) = {}
}