package neilw4.c4scala.state

import android.os.{Parcel, Parcelable}
import neilw4.c4scala.R
import neilw4.c4scala.controller.ScalaAi

object Piece {
    // Randomly selected ID to represent a None: Option[Piece] in a parcel.
    val NONE_ID: Byte = 678.asInstanceOf[Byte]

    def read(source: Parcel) = source.readByte match {
        case BLANK.id => BLANK
        case YELLOW.id => YELLOW
        case RED.id => RED
        case NONE_ID => null
    }

    def write(piece: Piece, dest: Parcel): Unit = dest.writeByte(piece.id)
    def write(winner: Option[Piece], dest: Parcel): Unit = winner match {
        case None => dest.writeByte(NONE_ID)
        case Some(piece) => Piece.write(piece, dest)
    }
}

/** A piece or player. May be BLANK, RED or YELLOW. */
trait Piece {val id: Byte; val colour: Int; val win_text: Int; val opposite: Piece}
case object BLANK extends Piece {
    val id = 0.asInstanceOf[Byte]
    val colour = R.color.pretty_white
    val win_text = R.string.draw
    val opposite = BLANK
}
case object YELLOW extends Piece {
    val id = 1.asInstanceOf[Byte]
    val colour = R.color.mild_yellow
    val win_text = R.string.yellow_win
    val opposite = RED
}
case object RED extends Piece {
    val id = 2.asInstanceOf[Byte]
    val colour = R.color.chilled_red
    val win_text = R.string.red_win
    val opposite = YELLOW
}

object Board {
    val WIDTH = 7
    val HEIGHT = 6

    val CREATOR: Parcelable.Creator[Board] = new Parcelable.Creator[Board] {
        override def newArray(size: Int) = Array.fill[Board](size)(null)

        override def createFromParcel(source: Parcel) = source match {
            case null => new Board
            case _ => {
                val emptyBoard: Array[Array[Piece]] = Array.ofDim[Piece](Board.WIDTH, Board.HEIGHT)
                val board: Array[Array[Piece]] = emptyBoard.map(_.map(x => Piece.read(source).asInstanceOf[Piece]))
                val nextPiece = Piece.read(source)
                val winner = Option(Piece.read(source))
                new Board(board, nextPiece, winner)
            }
        }
    }
}

/** Represents a game board. */
class Board(board: Array[Array[Piece]], var nextPiece: Piece, var winner: Option[Piece]) extends ListenerManager[StateListener] with Parcelable {

    /** Store the height of each column to save lookup time. */
    val heights = board.map(_.takeWhile(BLANK !=).length)

    /** true if the AI is thinking and the board should not be modified. */
    var aiThinking: Boolean = false

    def this() = this(Array.fill[Piece](Board.WIDTH, Board.HEIGHT)(BLANK), YELLOW, None)

    override def writeToParcel(dest: Parcel, flags: Int) = {
        foreach((piece) => Piece.write(piece, dest))
        Piece.write(nextPiece, dest)
        Piece.write(winner, dest)
    }

    override def describeContents = 0

    /** Iterates over every cell. */
    def foreach(f: Piece => Unit) = board.foreach(_.foreach(f))

    def apply(i: Int) = board.apply(i)

    def canRemove(col: Int): Boolean = heights(col) > 0

    /** Removes a piece from a column. */
    def remove(col: Int): Boolean =
        if (canRemove(col)) {
            heights(col) -= 1
            val row = heights(col)
            board(col)(row) = BLANK
            nextPiece = nextPiece.opposite
            alertListeners(_.onBoardPieceChanged(col, row))
            true
        } else false

    def canAdd(col: Int) = heights(col) < board(0).length && !aiThinking

    /** Adds a piece to a column. */
    def add(col: Int) : Boolean =
        if (canAdd(col)) {
            val row = heights(col)
            board(col)(row) = nextPiece
            heights(col)+= 1
            nextPiece = nextPiece.opposite
            alertListeners(_.onBoardPieceChanged(col, row))
            true
        } else false

    def isFull: Boolean = !heights.exists(_ < Board.HEIGHT)

    /** Sets the winner, if one exists. */
    def setWinner(winner: Option[Piece]) = if(winner != this.winner) {
        this.winner = winner
        winner match {
            case Some(piece) => alertListeners(_.onGameEnd(piece))
            case None =>
        }
    }

    // Slightly hacky, but hey, we'll change it soon anyway.
    /** Should be called after add(col), unless the AI is calculating, because it is slow. */
    def checkWinner(lastCol: Int) = setWinner(new ScalaAi(this, null).checkWin(lastCol))

    def startedThinking() = if (!aiThinking) {
        aiThinking = true
        alertListeners(_.onStartThinking())
    }

    def stoppedThinking() = if (aiThinking) {
        aiThinking = false
        alertListeners(_.onStopThinking())
    }

    /** Deep copy. */
    override def clone = new Board(board.map(_.clone), nextPiece, winner)

    override def callAllListenerFunctions() = {
        Array.tabulate(Board.WIDTH, Board.HEIGHT) (
            (x, y) => alertListeners(_.onBoardPieceChanged(x, y))
        )
        aiThinking match {
            case false => alertListeners(_.onStopThinking())
            case true => alertListeners(_.onStartThinking())
        }
    }
}
