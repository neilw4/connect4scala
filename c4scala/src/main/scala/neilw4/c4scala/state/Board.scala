package neilw4.c4scala.state

import android.os.{Parcel, Parcelable}
import neilw4.c4scala.R

import scala.collection.mutable

object Piece {
    def read(source: Parcel) = source.readByte match {
        case BLANK.id => BLANK
        case YELLOW.id => YELLOW
        case RED.id => RED
    }

    def write(piece: Piece, dest: Parcel) = dest.writeByte(piece.id)
}

trait Piece {val id: Byte; val colour: Int; val opposite: Piece}
case object BLANK extends Piece {val id = 0.asInstanceOf[Byte]; val colour = R.color.pretty_white; val opposite = BLANK}
case object YELLOW extends Piece {val id = 1.asInstanceOf[Byte]; val colour = R.color.mild_yellow; val opposite = RED}
case object RED extends Piece {val id = 2.asInstanceOf[Byte]; val colour = R.color.chilled_red; val opposite = YELLOW}

object Board {
    val TAG = this.getClass.toString

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
                new Board(board, nextPiece)
            }
        }
    }
}

class Board(board: Array[Array[Piece]], var nextPiece: Piece) extends Parcelable {

    def this() = this(Array.fill[Piece](Board.WIDTH, Board.HEIGHT)(BLANK), YELLOW)

    override def writeToParcel(dest: Parcel, flags: Int) = {
        iterate((piece) => Piece.write(piece, dest))
        Piece.write(nextPiece, dest)
    }

    override def describeContents = 0

    private val listeners: mutable.Set[StateListener] = new mutable.HashSet[StateListener]()

    val heights = board.map(_.takeWhile(BLANK !=).length)

    def iterate(f: Piece => Unit) = board.foreach(_.foreach(piece => f(piece)))

    def apply(i: Int) = board.apply(i)

    def attachListener(listener: StateListener) = {
        this.listeners += listener
    }

    def removeListener(listener: StateListener) = {
        this.listeners -= listener
    }

    def callAllListeners = {
        Array.tabulate(Board.WIDTH, Board.HEIGHT) (
            (x, y) => listeners.map {
                _.onBoardPieceChanged(x, y)
            }
        )
    }

    def canAdd(col: Int) = heights(col) < board(0).length

    def add(col: Int) : Boolean =
        if (canAdd(col)) {
            val row = heights(col)
            board(col)(row) = nextPiece
            heights(col)+= 1
            nextPiece = nextPiece.opposite
            if (listeners.size > 0) {
            }
            listeners.map(_.onBoardPieceChanged(col, row))
            true
        } else false

    def canRemove(col: Int): Boolean = heights(col) > 0

    def remove(col: Int): Boolean =
        if (canRemove(col)) {
            heights(col) -= 1
            val row = heights(col)
            board(col)(row) = BLANK
            nextPiece = nextPiece.opposite
            listeners.map(_.onBoardPieceChanged(col, row))
            true
        } else false

    def isFull: Boolean = !heights.exists(_ < Board.HEIGHT)

    // Deep copy.
    override def clone = new Board(board.map(_.clone()), nextPiece)
}
