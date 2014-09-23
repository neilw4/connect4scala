package neilw4.c4scala.controller

import neilw4.c4scala.state._
import neilw4.c4scala.util.SimpleAsyncTask

class Controller(state: State) {

    private var asyncAi: Option[AsyncAi] = None

    def onColumnSelected(col: Int) = if (state.aiThinking.isEmpty) makeMove(col)

    def newGame = {
        stopAiMove
        state.newGame
    }

    def makeMove(col: Int) = {
        stopAiMove
        if (state.board.winner.isEmpty) {
            state.board.add(col)
            state.board.checkWinner(col)
            if (state.board.winner.isEmpty && state.playerAi(state.board.nextPiece)) {
                startAiMove
            }
        }
    }

    def startAiMove = {
        stopAiMove
        state.startedThinking(state.board.nextPiece)
        val task = new AsyncAi(state.board, this)
        asyncAi = Some(task)
        task.execute(state.difficulty)
    }

    def stopAiMove = {
        if (asyncAi.isDefined) {
            asyncAi.get.cancel(true)
            asyncAi = None
        }
        state.stoppedThinking
    }

}

class AsyncAi(board: Board, controller: Controller) extends SimpleAsyncTask[Int, Int] {
    override def doInBackground(depth: Int): Int = new ScalaAi(board).adviseMove(depth)
    override def onPostExecute(col: Int) = {
        controller.stopAiMove
        controller.makeMove(col)
    }
    override def onCancelled(col: Int) = controller.stopAiMove
}
