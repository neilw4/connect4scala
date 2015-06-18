package neilw4.c4scala.controller

import neilw4.c4scala.state._
import neilw4.c4scala.util.{SimpleAsyncTask, Maths}

import scala.util.control.Breaks._

/**
 * AIs that run in the background should extend this class.
 */
abstract class AsyncAi(var board: Board, controller: Controller) extends SimpleAsyncTask[Int, Int] {
    override def doInBackground(depth: Int): Int = adviseMove(depth)
    override def onPostExecute(col: Int) = {
        if(board.canAdd(col)) {
            controller.makeMove(col)
        } else {
            android.util.Log.w(getClass.getSimpleName, "AI attempted impossible move  in  column " + col)
        }
    }

    /**
     * Core of the AI's functionality should be in here.
     * Returns the column number to move in.
     */
    def adviseMove(difficulty: Int): Int
}

object ScalaAi {
    val TAG = this.getClass.toString

    val WIN_SCORE = Int.MaxValue
    val LOSE_SCORE = -WIN_SCORE
    val DRAW_SCORE = 0

  // Bound on how far the eval function should look.
  val MAX_MOVES_AWAY = 5
}

// Note that this clones the board to avoid disturbing the original.
class ScalaAi(_board: Board, controller: Controller) extends AsyncAi(_board.clone, controller) {

    def adviseMove(difficulty: Int) = negamax(difficulty, None, -Int.MaxValue, Int.MaxValue)._1

    def negamax(depth: Int, lastCol: Option[Int], alpha: Int, beta: Int): (Int, Int) = {
        if (isCancelled) {
            // Stop immediately.
            return (-1, -1)
        }
        if (lastCol.isDefined) {
            val winner = checkWin(lastCol.get)
            if (winner.isDefined) {
                val score = winner.get match {
                    case DRAW => ScalaAi.DRAW_SCORE // Draw is equally bad for both players.
                    case _ => ScalaAi.LOSE_SCORE // Losing is bad.
                }
                // We are guaranteed to lose, so just play a random valid column.
                val bestCol = colsFromCentre.find(board.canAdd).getOrElse(-1)
                return (bestCol, score)
            }
        }

        if (depth == 0) {
            return (-1, eval(board.nextPlayer))
        }

        var bestScore = alpha
        var bestCol: Int = colsFromCentre.find(board.canAdd).getOrElse(-1)
        for (col <- colsFromCentre) {
            if(board.add(col)) {
                val score = -negamax(depth - 1, Some(col), -beta, -bestScore)._2
                board.remove(col)
                if (score > bestScore) {
                    bestScore = score
                    bestCol = col
                    if (score >= beta) {
                        //TODO: fix alpha-beta
                        //return (bestCol, score)
                    }
                }
            }
        }
        (bestCol, bestScore)
    }

    // Search columns in this order, as the middle columns are
    // often the best position, so they should be searched first.
    val colsFromCentre = List(3, 4, 2, 5, 1, 6, 0)

    /** A sequence of four cells in a line, with their coordinates and values. */
    class Sequence(val col: Int, val row: Int, val deltaCol: Int, val deltaRow: Int) extends IndexedSeq[(Int, Int, Piece)] {
        override def apply(offset: Int) = {
            val newCol = col + offset * deltaCol
            val newRow = row + offset * deltaRow
            (newCol, newRow, board(newCol)(newRow))
        }
        val length = 4
    }

    class HorizontalSequence(col: Int, row: Int) extends Sequence(col, row, 1, 0) {}
    class VerticalSequence(col: Int, row: Int) extends Sequence(col, row, 0, 1) {}
    class Diagonal1Sequence(col: Int, row: Int) extends Sequence(col, row, 1, 1) {}
    class Diagonal2Sequence(col: Int, row: Int) extends Sequence(col, row, -1, 1) {}

    /** Counts the number of pieces of a certain type in the sequence. */
    def getCount(sequence: Sequence, piece: Piece) = sequence.takeWhile(_._3 == piece).size

    private val horizontalSequences =
        for (col <- 0 to Board.WIDTH - 4; row <- 0 to Board.HEIGHT - 1)
        yield new HorizontalSequence(col, row)

    private val verticalSequences =
        for (col <- 0 to Board.WIDTH - 1; row <- 0 to Board.HEIGHT - 4)
        yield new VerticalSequence(col, row)

    private val diagonal1Sequences =
        for (col <- 0 to Board.WIDTH - 4; row <- 0 to Board.HEIGHT - 4)
        yield new Diagonal1Sequence(col, row)

    private val diagonal2Sequences =
        for (col <- 3 to Board.WIDTH - 1; row <- 0 to Board.HEIGHT - 4)
        yield new Diagonal2Sequence(col, row)

    val allSequences =
        horizontalSequences ++ verticalSequences ++ diagonal1Sequences ++ diagonal2Sequences

    /** All sequences containing a certain cell. */
    def sequencesContaining(col: Int, row: Int) =
        horizontalSequencesContaining(col, row) ++ verticalSequencesContaining(col, row) ++ diagonal1SequencesContaining(col, row) ++ diagonal2SequencesContaining(col, row)

    private def horizontalSequencesContaining(col: Int, row: Int): IndexedSeq[Sequence] =
        for (offset <- Maths.max(-3, -col) to Maths.min(0, Board.WIDTH - 4 - col))
        yield new HorizontalSequence(col + offset, row)

    private def verticalSequencesContaining(col: Int, row: Int): IndexedSeq[Sequence] =
        for (offset <- Maths.max(-3, -row) to Maths.min(0, Board.HEIGHT - 4 - row))
        yield new VerticalSequence(col, row + offset)

    private def diagonal1SequencesContaining(col: Int, row: Int): IndexedSeq[Sequence] =
        for (offset <- Maths.max(-3, -row, -col) to Maths.min(0, Board.HEIGHT - 4 - row, Board.WIDTH - 4 - col))
        yield new Diagonal1Sequence(col + offset, row + offset)

    private def diagonal2SequencesContaining(col: Int, row: Int): IndexedSeq[Sequence] =
        for (offset <- Maths.max(-3, -row, 1 + col - Board.WIDTH) to Maths.min(0, col - 3, Board.HEIGHT - 4 - row))
        yield new Diagonal2Sequence(col - offset, row + offset)

    /**
     * Checks to see if the game has ended.
     * @param lastCol the last column used.
     * @return None if the game has ended, DRAW for a draw, or the winning piece.
     */
    def checkWin(lastCol: Int): Option[Winner] = {
        if (board.isFull) {
            return Some(DRAW)
        }
        val row = board.heights(lastCol) - 1
        val piece = board(lastCol)(row)
        if (piece.isInstanceOf[Winner]) {
            for (sequence <- sequencesContaining(lastCol, row)) {
                if (sequence.forall(_._3 == piece)) {
                    return Some(piece.asInstanceOf[Winner])
                }
            }
        }
        None
    }

    /** @return the value of the board from the viewpoint of a piece. */
    def eval(viewPoint: Player): Int = {
        var totalScore: Int = 0

        for (sequence <- allSequences) {
            var score: Int = math.pow(Board.WIDTH, ScalaAi.MAX_MOVES_AWAY).toInt
            var player: Piece = BLANK
            breakable {
                for ((col, row, piece) <- sequence) {
                    if ((piece == BLANK && player == BLANK) || (player != BLANK  && piece == player.asInstanceOf[Player].opposite)) {
                        score = 0
                        break()
                    }
                    player = piece
                    breakable {
                        for (rowTemp <- row to 0) {
                            if (board(col)(rowTemp) == BLANK && row - rowTemp > ScalaAi.MAX_MOVES_AWAY) {
                                score /= Board.WIDTH
                            } else {
                                break()
                            }
                        }
                    }
                    val multiplier = player match {
                        case `viewPoint` => 1
                        case BLANK => 0
                        case _ => -1
                    }
                    totalScore += multiplier * score
                }
            }
        }
        totalScore
    }

}

object NativeAi {
    System.loadLibrary("C4Scala")

    @native def nativeAdviseMove(jBoard: Array[Byte], jHeights: Array[Int], jPieceCount: Int, difficulty: Int):  Int
}

class NativeAi(board: Board, controller: Controller) extends AsyncAi(board, controller) {
    override def adviseMove(difficulty: Int): Int = NativeAi.nativeAdviseMove(boardAsByteArray, board.heights.clone, pieceCount, difficulty)

    private lazy val boardAsByteArray: Array[Byte] = board.map({
        case BLANK => 0.toByte
        case p: Player => if (p == board.nextPlayer) 1.toByte else -1.toByte
    }).toArray[Byte]

    private lazy val pieceCount: Int = board.count(BLANK !=)
}
