package neilw4.c4scala.state

import android.os.{Parcel, Parcelable}
import scala.collection.mutable

object State {
    val KEY = "com.neilw4.c4scala.State"
    val CREATOR: Parcelable.Creator[State] = new Parcelable.Creator[State] {
        override def newArray(size: Int) = Array.fill[State](size)(null)
        override def createFromParcel(source: Parcel) = new State(source)
    }

    //TODO: make functional
    def mapFromParcel(source: Parcel) = {
        val m = new mutable.HashMap[Piece, Boolean]
        val size = source.readByte
        for (i <- 0 to size) {
            val piece = Piece.read(source)
            val isAi = source.readByte == 1
            m += (piece, isAi).asInstanceOf[(Piece, Boolean)]
        }
        m
    }
}

class State(var difficulty: Int, var playerAi: mutable.Map[Piece, Boolean], var board: Board) extends Parcelable {

    def this() = this(5, mutable.Map(RED -> true, YELLOW -> false), new Board)

    def this(source: Parcel) = this(source.readInt, State.mapFromParcel(source), Board.CREATOR.createFromParcel(source))

    override def writeToParcel(dest: Parcel, flags: Int) = {
        dest.writeInt(difficulty)
        dest.writeInt(playerAi.size)
        playerAi.foreach {
            case (piece, isAi) => {
                Piece.write(piece, dest)
                dest.writeByte(if (isAi) 1 else 0)
            }
        }
        board.writeToParcel(dest, flags)
    }

    override def describeContents = 0

    private val listeners: mutable.Set[StateListener] = mutable.HashSet()

    def attachListener(listener: StateListener) = {
        listeners += listener
        board.attachListener(listener)
    }

    def removeListener(listener: StateListener) = {
        listeners -= listener
        board.removeListener(listener)
    }

    def callAllListeners = {
        listeners.foreach {
            _.onDifficultyChanged(difficulty)
        }
        playerAi.foreach {
            case (piece, isAi) => listeners.foreach(_.onPlayerAiChanged(piece, isAi))
        }
        board.callAllListeners
    }

    def newGame = {
        board = new Board
        listeners.foreach(board.attachListener)
        board.callAllListeners
    }

    def setDifficulty(tDifficulty: Int) = if (tDifficulty != difficulty) {
        difficulty = tDifficulty
        listeners.foreach(_.onDifficultyChanged(difficulty))
    }

    def setPlayerAi(piece: Piece, isAi: Boolean) = if (isAi != playerAi(piece)) {
        playerAi(piece) = isAi
        listeners.foreach(_.onPlayerAiChanged(piece, isAi))
    }

    listeners.foreach(board.attachListener)
}

trait StateListener {
  def onDifficultyChanged(difficulty: Int)
  def onPlayerAiChanged(piece: Piece, isAi: Boolean)
  def onBoardPieceChanged(x: Int, y: Int)
  def onGameEnd(winner: Piece)
  def onStartThinking(aiPiece: Piece)
  def onStopThinking()
}
