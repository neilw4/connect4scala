package neilw4.c4scala.state

import android.os.{Parcel, Parcelable}
import scala.collection.mutable

/** Allows a set of listeners to be alerted. */
trait ListenerManager[L] {
    private val listeners: mutable.Set[L] = mutable.HashSet()

    def attachListeners(listeners: Iterable[L]): Unit = this.listeners ++= listeners
    def attachListener(listener: L): Unit = listeners += listener
    def removeListener(listener: L): Unit = listeners -= listener
    def clearListeners(): Unit = listeners.clear()
    protected def alertListeners(fn: (L) => Unit): Unit = listeners.foreach(fn)

    def callAllListenerFunctions()
}

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

/**
 * All mutable state should be stored here.
 * @param difficulty how hard the AI should be.
 * @param playerAi stores whether a player is human or AI.
 * @param board the state of the game board.
 */
class State(var difficulty: Int, var playerAi: mutable.Map[Piece, Boolean], var board: Board) extends ListenerManager[StateListener] with Parcelable {

    def this() = this(4, mutable.Map(RED -> true, YELLOW -> false), new Board)

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

    /** Replaces the board with a new one. */
    def newGame() = {
        board.clearListeners()
        board = new Board
        alertListeners(board.attachListener(_))
        board.callAllListenerFunctions
    }

    def setDifficulty(difficulty: Int) = if (this.difficulty != difficulty) {
        this.difficulty = difficulty
        alertListeners(_.onDifficultyChanged(difficulty))
    }

    def setPlayerAi(piece: Piece, isAi: Boolean) = if (isAi != playerAi(piece)) {
        playerAi(piece) = isAi
        alertListeners(_.onPlayerAiChanged(piece, isAi))
    }

    def togglePlayerAi(player: Piece) = setPlayerAi(player, !playerAi(player))

    override def attachListeners(listeners: Iterable[StateListener]) = {
        super.attachListeners(listeners)
        board.attachListeners(listeners)
    }

    override def attachListener(listener: StateListener) = {
        super.attachListener(listener)
        board.attachListener(listener)
    }

    override def removeListener(listener: StateListener) = {
        super.removeListener(listener)
        board.removeListener(listener)
    }

    override def clearListeners() = {
        super.clearListeners()
        board.clearListeners()
    }

    override def callAllListenerFunctions() = {
        alertListeners(_.onDifficultyChanged(difficulty))

        playerAi.foreach {
            case (piece, isAi) => alertListeners(_.onPlayerAiChanged(piece, isAi))
        }
        board.callAllListenerFunctions()
    }
}

trait StateListener {
  def onDifficultyChanged(difficulty: Int)
  def onPlayerAiChanged(piece: Piece, isAi: Boolean)
  def onBoardPieceChanged(x: Int, y: Int)
  def onGameEnd(winner: Piece)
  def onStartThinking()
  def onStopThinking()
}
