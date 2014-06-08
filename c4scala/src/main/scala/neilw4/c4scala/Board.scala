package neilw4.c4scala

import android.os.Parcelable
import android.os.Parcel
import scala.collection.mutable

trait Piece {def id: Byte; def resource: Int}
case object BLANK extends Piece {val id = 0.asInstanceOf[Byte]; val resource = R.drawable.blank}
case object YELLOW extends Piece {val id = 1.asInstanceOf[Byte]; val resource = R.drawable.yellow}
case object RED extends Piece {val id = 2.asInstanceOf[Byte]; val resource = R.drawable.red}

object Board {
    val KEY = "com.neilw4.c4scala.Board"
    val CREATOR: Parcelable.Creator[Board] = new Parcelable.Creator[Board] {
        override def newArray(size: Int) = Array.fill[Board](size)(null)

        override def createFromParcel(source: Parcel) = source match {
            case null => new Board
            case _ => new Board(source)
        }
    }
}

class Board(val width: Int, val height: Int) extends Parcelable {

    def this() = this(7, 6)

    def this(source: Parcel) = {
        this(source.readInt, source.readInt)
        _board = _board.map(_.map(x => (source.readByte match {
            case BLANK.id => BLANK
            case YELLOW.id => YELLOW
            case RED.id => RED
        }).asInstanceOf[Piece]))
        heights = _board.map(_.takeWhile(BLANK !=).length)
    }

    override def writeToParcel(dest: Parcel, flags: Int) = {
        dest.writeInt(width)
        dest.writeInt(height)
        _board.map(_.map(piece => dest.writeByte(piece.id)))
    }

    override def describeContents = 0

    private var listeners: mutable.Set[StateListener] = null

    var _board = Array.fill[Piece](width, height)(BLANK)
    def board = _board
    private var heights = Array.fill[Int](width)(0)

    def attachListeners(listeners: mutable.Set[StateListener]) = {
        this.listeners = listeners
    }

    def callAllListeners = {
        Array.tabulate(width, height) (
            (x, y) => listeners.map {
                _.onBoardPieceChanged(_board(x)(y), x, y)
            }
        )
    }

    def canAdd(piece: Piece, x: Int) =
        piece != BLANK && heights(x) < _board(0).length

    def add(piece: Piece, x: Int) : Boolean =
        if (canAdd(piece, x)) {
            _board(x)(heights(x)) = piece
            heights(x)+= 1
            true
        } else false

}
