package neilw4.c4scala.controller

import neilw4.c4scala.state._
import neilw4.c4scala.util.Maths

import scala.util.control.Breaks._


trait Ai {
        def adviseMove(depth: Int): Int
}

object ScalaAi {
  val TAG = this.getClass.toString

  val WIN = Int.MaxValue
  val LOSE = -WIN
  val DRAW = 0
  val NO_WIN = 0

  // Eval function uses this
  val MAX_MOVES_AWAY = 5
}

class ScalaAi(var board: Board) extends Ai {
    board = board.clone()

    def adviseMove(depth: Int) = negamax(depth, None, -Int.MaxValue, Int.MaxValue)._1

    def negamax(depth: Int, lastCol: Option[Int], alpha: Int, beta: Int): (Int, Int) = {
        if (lastCol.isDefined) {
            val winner = checkWin(lastCol.get)
            if (winner.isDefined) {
                val score = winner.get match {
                    case BLANK => ScalaAi.DRAW // Draw is equally bad for both players.
                    case _ => ScalaAi.LOSE // Losing is bad.
                }
                println(s"$depth endgame score $score, (eval ${eval(board.nextPiece)})")
                return (-1, score)
            }
        }

        if (depth == 0) {
            println(s"$depth leaf eval_score ${eval(board.nextPiece)}")
            return (-1, eval(board.nextPiece))
        }

        var bestScore = alpha
        var bestCol: Int = -1
        for (col <- colsFromCentre) {
            if(board.add(col)) {
                val score = -negamax(depth - 1, Some(col), -beta, -bestScore)._2
                board.remove(col)
                println(s"$depth col $col score $score best $bestScore")
                if (score > bestScore) {
                    bestScore = score
                    bestCol = col
                    if (score >= beta) {
                        println(s"$depth col $col BETA $beta score $score")
                        //TODO: fix alpha-beta
                        //return (bestCol, score)
                    }
                }
            }
        }
        println(s"$depth ALL col $bestCol score $bestScore")
        return (bestCol, bestScore)
    }

    val colsFromCentre = List(3, 4, 2, 5, 1, 6, 0)

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

    //TODO: memoise
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

    def sequencesContaining(col: Int, row: Int) =
        horizontalSequencesContaining(col, row) ++ verticalSequencesContaining(col, row) ++ diagonal1SequencesContaining(col, row) ++ diagonal2SequencesContaining(col, row)


    def checkWin(col: Int): Option[Piece] = {
        if (board.isFull) {
            return Some(BLANK)
        }
        val row = board.heights(col) - 1
        val piece = board(col)(row)

        for (sequence <- sequencesContaining(col, row)) {
            if (sequence.forall(_._3 == piece)) {
                return Some(piece)
            }
        }
        return None
    }

    // Returns the value of the board from the viewpoint of a piece.
    def eval(viewPoint: Piece): Int = {
        var totalScore: Int = 0

        for (sequence <- allSequences) {
            var score: Int = math.pow(Board.WIDTH, ScalaAi.MAX_MOVES_AWAY).toInt
            var player: Piece = BLANK
            breakable {
                for ((col, row, piece) <- sequence) {
                    if (piece == player.opposite) {
                        score = 0
                        break
                    }
                    player = piece
                    breakable {
                        for (rowTemp <- row to 0) {
                            if (board(col)(rowTemp) == BLANK && row - rowTemp > ScalaAi.MAX_MOVES_AWAY) {
                                score /= Board.WIDTH
                            } else {
                                break
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
