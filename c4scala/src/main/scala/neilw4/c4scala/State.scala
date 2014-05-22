package neilw4.c4scala

import scala.collection.mutable.Set
import android.os.Parcelable
import android.os.Parcel

object State {
    val KEY = "com.neilw4.c4scala.State"
    val CREATOR: Parcelable.Creator[State] = new Parcelable.Creator[State] {
        override def newArray(size: Int) = Array.fill[State](size)(null)
        override def createFromParcel(source: Parcel) = new State(source)
    }
}

trait StateListener {
    def onDifficultyChanged(difficulty: Int)
    def onPlayerAiChanged(playerAi: Boolean, index: Int)
    def onBoardPieceChanged(piece: Piece, x: Int, y: Int)
}

class State(private var _difficulty: Int, private var _playerAi: Array[Boolean], private var _board: Board) extends Parcelable {

    def this() = this(50, Array(false, true), new Board)

    def this(source: Parcel) = this(source.readInt, Array.tabulate[Boolean](source.readInt())(_ => (source.readByte() == 1)), Board.CREATOR.createFromParcel(source))

    override def writeToParcel(dest: Parcel, flags: Int) = {
        dest.writeInt(difficulty)
        dest.writeInt(_playerAi.length)
        _playerAi.map(x => dest.writeByte(if (x) 1 else 0))
        _board.writeToParcel(dest, flags)
    }

    override def describeContents = 0

    private val listeners: Set[StateListener] = Set()

    def attachListener(listener: StateListener) = {
        listeners += listener
    }

    def removeListener(listener: StateListener) = {
        listeners -= listener
    }

    def callAllListeners = {
        listeners.map (_.onDifficultyChanged(difficulty))
        Array.tabulate(_playerAi.length){i => listeners.map (_.onPlayerAiChanged(_playerAi(i), i))}
        _board.callAllListeners
    }

    def newGame = {
        _board = new Board
        _board.attachListeners(listeners)
        _board.callAllListeners
    }

    def difficulty = _difficulty
    def difficulty_= (tDifficulty: Int) = if (tDifficulty != _difficulty) {
        _difficulty = tDifficulty
        listeners map {_.onDifficultyChanged(_difficulty)}
    }

    def playerAi = _playerAi
    def setPlayerAi (tPlayerAi: Boolean, index: Int) = if (tPlayerAi != _playerAi(index)) {
        _playerAi(index) = tPlayerAi
        listeners.map(_.onPlayerAiChanged(_playerAi(index), index))
    }

    _board.attachListeners(listeners)
    def board = _board.board

    def width = _board.width
    def height = _board.height
}
