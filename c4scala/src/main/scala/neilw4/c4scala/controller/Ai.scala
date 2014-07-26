package neilw4.c4scala.controller

import neilw4.c4scala.state._
import neilw4.c4scala.util.Maths

import scala.collection.Seq
import scala.util.control.Breaks._


trait Ai {
        def adviseMove(depth: Int): Int
}

object ScalaAi {
  val TAG = this.getClass.toString

  val WIN = Int.MaxValue
  val LOSE = Int.MinValue
  val DRAW = Int.MinValue + 1
  val NO_WIN = 0

  // Eval function uses this
  val MAX_MOVES_AWAY = 5
}

class ScalaAi(_board: Board) extends Ai {
    val board = _board.clone()
    val AiPiece = board.nextPiece

    def adviseMove(depth: Int) = negamax(depth, AiPiece, Int.MinValue, Int.MaxValue)._1

    // Implementation of the negamax search algorithm.
    // Returns (col, value).
    def negamax(depth: Int, player: Piece, alpha: Int, beta: Int): (Int, Int) = {
        if (depth == 0) {
            if (player == AiPiece) {
                return (-1, eval())
            } else {
                return (-1, -eval())
            }
        }
        var newAlpha = alpha
        var bestVal: Int = Int.MinValue
        var bestCol: Int = -1
        // Centre pieces are more likely to be good.
        // col = 3, 4, 2, 5, 1, 6, 0.
        for (col <- new ColsFromCentre(Board.WIDTH)) {
            if (board.add(col)) {
                val endGame: Int = checkWin(col)
                if (endGame != ScalaAi.NO_WIN) {
                    // Game has ended.
                    return (col, -endGame)
                }
                // Game hasn't ended.
                val value = -negamax(depth - 1, player.opposite, -beta, -newAlpha)._2
                if (value > bestVal) {
                    bestVal = value
                    bestCol = col
                    if (value > newAlpha) {
                        newAlpha = value
                        if (newAlpha >= beta) {
                            board.remove(col)
                            return (col, bestVal)
                        }
                    } else {
                        print(value + " value <= alpha " + newAlpha)
                    }
                }
                board.remove(col)
            }
        }
        (bestCol, bestVal)
    }

    class ColsFromCentre(val top: Int) extends Iterator[Int] {
        var current = top / 2

        def hasNext() = current < top

        def next() = {
            if (current <= top / 2) {
                current = top - current
            } else {
                current = top - current - 1
            }
            current
        }
    }

    class Sequence(val col: Int, val row: Int, val deltaCol: Int, val deltaRow: Int) extends IndexedSeq[(Int, Int, Piece)] {

        override def apply(offset: Int) = {
            val newCol = col + offset * deltaCol
            val newRow = row + offset * deltaRow
            (newCol, newRow, board(newCol)(newRow))
        }

        val maxWidth = deltaCol match {
            case -1 => col
            case 0 => 4
            case 1 => Board.WIDTH - col
        }

        val maxHeight = deltaRow match {
            case -1 => row
            case 0 => 4
            case 1 => Board.HEIGHT - row
        }

        val length = Maths.min(4, maxWidth, maxHeight)
    }

    class HorizontalSequence(col: Int, row: Int) extends Sequence(col, row, 0, 1) {}

    class VerticalSequence(col: Int, row: Int) extends Sequence(col, row, 1, 0) {}

    class Diagonal1Sequence(col: Int, row: Int) extends Sequence(col, row, 1, 1) {}

    class Diagonal2Sequence(col: Int, row: Int) extends Sequence(col, row, -1, 1) {}


    def getCount(sequence: Sequence, piece: Piece) = sequence.takeWhile(_._3 == piece).size

    private val horizontalSequences =
        for (col <- 0 to Board.WIDTH - 4; row <- 0 to Board.HEIGHT)
        yield new HorizontalSequence(col, row)

    private val verticalSequences =
        for (col <- 0 to Board.WIDTH; row <- 0 to Board.HEIGHT - 4)
        yield new VerticalSequence(col, row)

    private val diagonal1Sequences =
        for (col <- 0 to Board.WIDTH - 4; row <- 0 to Board.HEIGHT - 4)
        yield new Diagonal1Sequence(col, row)

    private val diagonal2Sequences =
        for (col <- 3 to Board.WIDTH; row <- 0 to Board.HEIGHT - 4)
        yield new Diagonal2Sequence(col, row)

    val allSequences =
        horizontalSequences ++ verticalSequences ++ diagonal1Sequences ++ diagonal2Sequences

    //TODO: memoise
    private def horizontalSequencesContaining(col: Int, row: Int): IndexedSeq[Sequence] =
        for (offset <- Maths.max(-3, -col) to Maths.min(3, Board.WIDTH - col - 1))
        yield new HorizontalSequence(col + offset, row)

    private def verticalSequencesContaining(col: Int, row: Int): IndexedSeq[Sequence] =
        for (offset <- Maths.max(-3, -row) to Maths.min(3, Board.HEIGHT - row - 1))
        yield new VerticalSequence(col, row + offset)

    private def diagonal1SequencesContaining(col: Int, row: Int): IndexedSeq[Sequence] =
        for (offset <- Maths.max(-3, -row, -col) to Maths.min(3, Board.HEIGHT - row - 1, Board.WIDTH - col - 1))
        yield new Diagonal1Sequence(col, row)

    private def diagonal2SequencesContaining(col: Int, row: Int): IndexedSeq[Sequence] =
        for (offset <- Maths.max(-3, Board.HEIGHT - row - 1, -col) to Maths.min(3, -row, Board.WIDTH - col))
        yield new Diagonal2Sequence(col, row)

    def sequencesContaining(col: Int, row: Int) =
        horizontalSequencesContaining(col, row) ++ verticalSequencesContaining(col, row) ++ diagonal1SequencesContaining(col, row) ++ diagonal2SequencesContaining(col, row)


    // Checks for a win. Returns INT_MAX for a win,
    // INT_MIN + 1 if it is a draw and 0 if the game hasn't ended.
    def checkWin(col: Int): Int = {
        if (board.isFull) {
            return ScalaAi.DRAW
        }
        val row = board.heights(col) - 1

        for (sequence <- sequencesContaining(col, row)) {
//            var y: (Int, Int, Piece) = null
//            for (x <- sequence) {
//                y = x
//            }

            if (sequence.forall(_._3 == AiPiece)) {
                return ScalaAi.WIN
            } else if (sequence.forall(_._3 == AiPiece.opposite)) {
                return ScalaAi.LOSE
            }
        }
        return ScalaAi.NO_WIN
    }

    // Returns the value of the board from the viewpoint of AiPiece.
    def eval(): Int = {
        var score: Int = 0

        for (sequence <- allSequences) {
            var movesAwayScore: Int = math.pow(Board.WIDTH, ScalaAi.MAX_MOVES_AWAY).toInt
            var player: Piece = BLANK
            breakable {
                for ((col, row, piece) <- sequence) {
                    if (piece == player.opposite) {
                        movesAwayScore = 0
                        break
                    }
                    player = piece
                    breakable {
                        for (rowTemp <- row to 0) {
                            if (board(col)(rowTemp) == BLANK && row - rowTemp > ScalaAi.MAX_MOVES_AWAY) {
                                movesAwayScore /= Board.WIDTH
                            } else {
                                break
                            }
                        }
                    }
                    if (player == AiPiece) {
                        score += movesAwayScore
                    } else {
                        score -= movesAwayScore
                    }
                }
            }
        }
        score
    }

}
