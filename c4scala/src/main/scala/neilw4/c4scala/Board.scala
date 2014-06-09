package neilw4.c4scala

import android.os.Parcelable
import android.os.Parcel
import scala.collection.mutable

trait Piece {def id: Byte; def resource: Int}
case object BLANK extends Piece {val id = 0.asInstanceOf[Byte]; val resource = R.drawable.blank}
case object YELLOW extends Piece {val id = 1.asInstanceOf[Byte]; val resource = R.drawable.yellow}
case object RED extends Piece {val id = 2.asInstanceOf[Byte]; val resource = R.drawable.red}

object Board {
    val TAG = this.getClass.toString
    val CREATOR: Parcelable.Creator[Board] = new Parcelable.Creator[Board] {
        override def newArray(size: Int) = Array.fill[Board](size)(null)

        override def createFromParcel(source: Parcel) = source match {
            case null => new Board
            case _ => {
                val width = source.readInt()
                val height = source.readInt()
                val emptyBoard: Array[Array[Piece]] = Array.ofDim[Piece](width, height)

                // TODO: find out why this can't be simpler
                val board: Array[Array[Piece]] = emptyBoard.map(_.map(x => {
                    val v: Piece = source.readByte match {
                        case BLANK.id => BLANK
                        case YELLOW.id => YELLOW
                        case RED.id => RED
                    }
                    v
                }))
                new Board(width, height, board)
            }
        }
    }
}

class Board(val width: Int, val height: Int, board: Array[Array[Piece]]) extends Parcelable {

    def this(width: Int, height: Int) = this(width, height, Array.fill[Piece](width, height)(BLANK))
    def this() = this(7, 6)

    override def writeToParcel(dest: Parcel, flags: Int) = {
        dest.writeInt(width)
        dest.writeInt(height)
        iterate((piece) => dest.writeByte(piece.id))
    }

    override def describeContents = 0

    private var listeners: mutable.Set[StateListener] = null

    private val heights = board.map(_.takeWhile(BLANK !=).length)

    def iterate(f: Piece => Unit) = board.foreach(_.foreach(piece => f(piece)))

    def apply(i: Int) = board.apply(i)

    def attachListeners(listeners: mutable.Set[StateListener]) = {
        this.listeners = listeners
    }

    def callAllListeners = {
        Array.tabulate(width, height) (
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

}
