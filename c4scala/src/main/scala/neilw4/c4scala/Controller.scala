package neilw4.c4scala

import android.os.Bundle
import android.view.MenuInflater
import android.view.Menu
import android.util.Log

object Controller {
  def TAG = this.getClass().toString()
}

class Controller(mState: State) extends UiCallback {
    def onColumnSelected(column: Int) = {}
}