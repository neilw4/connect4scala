package neilw4.c4scala

import android.os.Parcelable
import android.os.Parcel
import scala.collection.mutable

trait Piece {val id: Byte; val resource: Int; val opposite: Piece}
case object BLANK extends Piece {val id = 0.asInstanceOf[Byte]; val resource = R.drawable.blank; val opposite = BLANK}
case object YELLOW extends Piece {val id = 1.asInstanceOf[Byte]; val resource = R.drawable.yellow; val opposite = RED}
case object RED extends Piece {val id = 2.asInstanceOf[Byte]; val resource = R.drawable.red; val opposite = YELLOW}

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

                // TODO: find out why this can't be simpler
                val board: Array[Array[Piece]] = emptyBoard.map(_.map(x => {
                    val v: Piece = source.readByte match {
                        case BLANK.id => BLANK
                        case YELLOW.id => YELLOW
                        case RED.id => RED
                    }
                    v
                }))
                new Board(board)
            }
        }
    }
}

class Board(board: Array[Array[Piece]]) extends Parcelable {

    def this() = this(Array.fill[Piece](Board.WIDTH, Board.HEIGHT)(BLANK))

    override def writeToParcel(dest: Parcel, flags: Int) = {
        iterate((piece) => dest.writeByte(piece.id))
    }

    override def describeContents = 0

    private var listeners: mutable.Set[StateListener] = null

    val heights = board.map(_.takeWhile(BLANK !=).length)

    def iterate(f: Piece => Unit) = board.foreach(_.foreach(piece => f(piece)))

    def apply(i: Int) = board.apply(i)

    def attachListeners(listeners: mutable.Set[StateListener]) = {
        this.listeners = listeners
    }

    def callAllListeners = {
        Array.tabulate(Board.WIDTH, Board.HEIGHT) (
            (x, y) => listeners.map {
                _.onBoardPieceChanged(board(x)(y), x, y)
            }
        )
    }

    def canAdd(piece: Piece, x: Int) =
        piece != BLANK && heights(x) < board(0).length

    def add(piece: Piece, x: Int) : Boolean =
        if (canAdd(piece, x)) {
            board(x)(heights(x)) = piece
            heights(x)+= 1
            true
        } else false

    def canRemove(x: Int): Boolean = heights(x) > 0

    def remove(x: Int): Boolean =
        if (canRemove(x)) {
            heights(x) -= 1
            board(x)(heights(x)) = BLANK
            true
        } else false

    def isFull(): Boolean = {
        for (colHeight <- heights) {
            if (colHeight < Board.HEIGHT) {
                return false
            }
        }
        return true
    }
}
