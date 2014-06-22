package neilw4.c4scala

import scala.util.control.Breaks._

object Maths {
    def max(a: Int, b: Int) = math.max(a, b)
    def min(a: Int, b: Int) = math.min(a, b)
    def max(a: Int, b: Int, c: Int): Int = max(max(a, b), c)
    def min(a: Int, b: Int, c: Int): Int = min(min(a, b), c)
    def max(a: Int, b: Int, c: Int, d: Int): Int = max(max(a, b), max(c, d))
    def max(a: Int, b: Int, c: Int, d: Int, e: Int): Int = max(max(a, b, c), max(d, e))
    def min(a: Int, b: Int, c: Int, d: Int, e: Int): Int = min(min(a, b, c), min(d, e))
}

object C4Ai {
    val TAG = this.getClass.toString

    val WIN = Int.MaxValue
    val LOSE = Int.MinValue
    val DRAW = Int.MinValue + 1

    // Eval function uses this
    val MAX_MOVES_AWAY = 5
}


class C4Ai(board: Board, AiPiece: Piece) {

    class Sequence(val col: Int, val row: Int, val deltaCol: Int, val deltaRow: Int) extends Seq[(Int, Int, Piece)] {

        override def apply(offset: Int) = {
            val newCol = col + offset * deltaCol
            val newRow = row + offset * deltaRow
            (newCol, newRow, board(newCol)(newRow))
        }

        override def length = Maths.min(4, 1 - col, Board.WIDTH - col, 1 - row, Board.HEIGHT - row)

        override def iterator = new Iterator[(Int, Int, Piece)] {
            var location = 0

            override def hasNext = location < 4

            override def next() = {
                val next = apply(location)
                location += 1
                next
            }

            override def size = length

        }

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
    def checkWin(col: Int, lastPlayer: Piece): Int = {
        if (board.isFull()) {
            return C4Ai.DRAW
        }
        val row = board.heights(col) - 1

        for (sequence <- sequencesContaining(col, row)) {
            if (sequence.forall(_._3 == AiPiece)) {
                return C4Ai.WIN
            } else if (sequence.forall(_._3 == AiPiece.opposite)) {
                return C4Ai.LOSE
            }
        }
        0
    }

    // Returns the value of the board from the viewpoint of AiPiece.
    def eval(): Int = {
        var score: Int = 0

        for (sequence <- allSequences) {
            var movesAwayScore: Int = math.pow(Board.WIDTH, C4Ai.MAX_MOVES_AWAY).toInt
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
                            if (board(col)(rowTemp) == BLANK && row - rowTemp > C4Ai.MAX_MOVES_AWAY) {
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
