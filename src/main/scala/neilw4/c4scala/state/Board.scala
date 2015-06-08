package neilw4.c4scala.state

import android.os.{Parcel, Parcelable}
import neilw4.c4scala.R
import neilw4.c4scala.controller.ScalaAi

object ParcelableEnum {
    // Randomly selected ID to represent a None: Option[Piece] in a parcel.
    val NONE_ID: Byte = 57.asInstanceOf[Byte]

    def read(source: Parcel) = source.readByte() match {
        case BLANK.id => BLANK
        case YELLOW.id => YELLOW
        case RED.id => RED
        case DRAW.id => DRAW
        case NONE_ID  => null
    }

    def write(piece: Option[ParcelableEnum], dest: Parcel): Unit = piece match {
        case Some(piece) => dest.writeByte(piece.id)
        case None => dest.writeByte(NONE_ID)
    }

    def write(piece: ParcelableEnum, dest: Parcel): Unit = write(Option(piece), dest)
}

object Piece {
    def read(source: Parcel) = ParcelableEnum.read(source).asInstanceOf[Piece]
    def write(piece: Piece, dest: Parcel) = ParcelableEnum.write(piece, dest)
}

object Player {
    def read(source: Parcel) = ParcelableEnum.read(source).asInstanceOf[Player]
    def write(player: Player, dest: Parcel) = ParcelableEnum.write(player, dest)
}

object Winner {
    def read(source: Parcel) = Option(ParcelableEnum.read(source).asInstanceOf[Winner])
    def write(winner: Option[Winner], dest: Parcel) = ParcelableEnum.write(winner, dest)
}

trait ParcelableEnum {val id: Byte}
/** Drawable piece may be BLANK, RED or YELLOW. */
trait Piece extends ParcelableEnum {val colour: Int}
/** Winner of the game may be DRAW, RED or YELLOW. */
trait Winner extends ParcelableEnum {val win_text: Int}
/** A player. May be RED or YELLOW. */
trait Player extends Piece with Winner {val opposite: Player}

case object BLANK extends Piece {
    val id: Byte = 0
    val colour = R.color.pretty_white
}
case object YELLOW extends Player {
    val id: Byte = 1
    val colour = R.color.mild_yellow
    val win_text = R.string.yellow_win
    val opposite = RED
}
case object RED extends Player {
    val id: Byte = 2
    val colour = R.color.chilled_red
    val win_text = R.string.red_win
    val opposite = YELLOW
}
case object DRAW extends Winner {
    val id: Byte = 3
    val win_text = R.string.draw
}

object Board {
    val WIDTH = 7
    val HEIGHT = 6

    val CREATOR: Parcelable.Creator[Board] = new Parcelable.Creator[Board] {
        override def newArray(size: Int) = Array.fill[Board](size)(null)

        override def createFromParcel(source: Parcel) = source match {
            case null => new Board
            case _ =>
                val emptyBoard: Array[Array[Piece]] = Array.ofDim[Piece](Board.WIDTH, Board.HEIGHT)
                val board: Array[Array[Piece]] = emptyBoard.map(_.map(x => Piece.read(source)))
                val nextPlayer = Player.read(source)
                val winner = Winner.read(source)
                new Board(board, nextPlayer, winner)
        }
    }
}

/** Represents a game board. */
class Board(board: Array[Array[Piece]], var nextPlayer: Player, var winner: Option[Winner]) extends ListenerManager[StateListener] with Parcelable {

    /** Store the height of each column to save lookup time. */
    val heights = board.map(_.takeWhile(BLANK !=).length)

    /** true if the AI is thinking and the board should not be modified. */
    var aiThinking: Boolean = false

    def this() = this(Array.fill[Piece](Board.WIDTH, Board.HEIGHT)(BLANK), YELLOW, None)

    override def writeToParcel(dest: Parcel, flags: Int) = {
        foreach((piece) => Piece.write(piece, dest))
        Player.write(nextPlayer, dest)
        Winner.write(winner, dest)
    }

    override def describeContents = 0

    /** Iterates over every cell. */
    def foreach(f: Piece => Unit) = board.foreach(_.foreach(f))

    def  apply(col: Int) = board(col)

    def canRemove(col: Int): Boolean = heights(col) > 0

    /** Removes a piece from a column. */
    def remove(col: Int): Boolean =
        if (canRemove(col)) {
            heights(col) -= 1
            val row = heights(col)
            board(col)(row) = BLANK
            nextPlayer = nextPlayer.opposite
            alertListeners(_.onBoardPieceChanged(col, row))
            true
        } else false

    def canAdd(col: Int) = heights(col) < board(0).length && !aiThinking

    /** Adds a piece to a column. */
    def add(col: Int) : Boolean =
        if (canAdd(col)) {
            val row = heights(col)
            board(col)(row) = nextPlayer
            heights(col)+= 1
            nextPlayer = nextPlayer.opposite
            alertListeners(_.onBoardPieceChanged(col, row))
            true
        } else false

    def isFull: Boolean = !heights.exists(_ < Board.HEIGHT)

    /** Sets the winner, if one exists. */
    def setWinner(winner: Option[Winner]) = if(winner != this.winner) {
        this.winner = winner
        winner match {
            case Some(piece) => alertListeners(_.onGameEnd(piece))
            case None =>
        }
    }

    // Slightly hacky, but hey, we'll change it soon anyway.
    /** Should be called after add(col), unless the AI is calculating, because it is slow. */
    def checkWinner(lastCol: Int) = setWinner(new ScalaAi(this, null).checkWin(lastCol))

    def startAiThinking() = {
        stopAiThinking()
        aiThinking = true
        alertListeners(_.onStartAiThinking())
    }

    def stopAiThinking() = if (aiThinking) {
        aiThinking = false
        alertListeners(_.onStopAiThinking())
    }

    /** Deep copy. */
    override def clone = new Board(board.map(_.clone), nextPlayer, winner)

    override def callAllListenerFunctions() = {
        Array.tabulate(Board.WIDTH, Board.HEIGHT) (
            (x, y) => alertListeners(_.onBoardPieceChanged(x, y))
        )
        aiThinking match {
            case false => alertListeners(_.onStopAiThinking())
            case true => alertListeners(_.onStartAiThinking())
        }
    }
}
