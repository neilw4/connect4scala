#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include <assert.h>

#include "C4Ai.h"

void resetBoard(Board* board) {
    memset(&(board->board), BLANK, sizeof(Piece) * WIDTH * HEIGHT);
    memset(&board->pieceHeight, 0, sizeof(Piece) * WIDTH);
    board->pieceCount = 0;
}

// Adds a piece to the board. Returns false if this cannot be done.
bool addPiece(Board* board, int col, Piece piece) {
    if ((board->pieceHeight[col]) >= HEIGHT - 1) {
        // Column is full.
        return false;
    }
    board->board[col][(board->pieceHeight[col])] = piece;
    board->pieceHeight[col]++;
    board->pieceCount++;
    return true;
}

// Removes the most recently added piece from the column.
void removePiece(Board* board, int col) {
    board->pieceHeight[col]--;
    board->board[col][(board->pieceHeight[col])] = BLANK;
    board->pieceCount--;
}

// Counts the number of matching pieces in a line.
int getCount(Board* board, int row, int col, int deltaRow, int deltaCol, Piece piece) {
    int count = 0;
    while(board->board[col][row] == piece
            && col < WIDTH && row < HEIGHT
            && col >= 0 && row >= 0) {
        count++;

        col+=deltaCol;
        row += deltaRow;
    }
    return count;
}

// Checks for a win. Returns INT_MAX for a win,
// INT_MIN + 1 if it is a draw and 0 if the game hasn't ended.
int checkWin(Board* board, int col, Piece lastPlayer) {
    if (board->pieceCount == 42) {
        return INT_MIN + 1;
    }
    int row = board->pieceHeight[col] - 1;
    if (row == -1) {
        row++;
        fprintf(stderr, "row == -1???\n");
    }
    // Checks the 4 possible around (col, row). Must be greater than 3 because it does not count (col, row).
    if (getCount(board, row + 1, col + 1, 1, 1, lastPlayer) + getCount(board, row - 1, col - 1, -1, -1, lastPlayer) >= 3 // Diagonal 1.
        || getCount(board, row + 1, col - 1, 1, -1, lastPlayer) + getCount(board, row - 1, col + 1, -1, 1, lastPlayer) >= 3 // Diagonal 2.
        || getCount(board, row + 1, col, 1, 0, lastPlayer) + getCount(board, row - 1, col, -1, 0, lastPlayer) >= 3 // Vertical.
        || getCount(board, row, col + 1, 0, 1, lastPlayer) + getCount(board, row, col - 1, 0, -1, lastPlayer) >= 3 // Horizontal
        ) {
        return INT_MAX;
    }
    return 0;
}

// Returns the value of the board from the viewpoint of ME.
int eval(Board* board) {
    int score = 0;

    int row, col, index, rowTemp;
    //For every possible vertical line.
    for(row = 0; row < HEIGHT - 3; row++){
        for(col = 0;col < HEIGHT; col++){
            // Multiplied by 7 for every extra move to get to the position.
            int movesAwayScore = 16807;
            Piece player = BLANK;
            // For every cell in the line.
            for (index = 0; index < 4; index++) {
                Piece value = board->board[col][row + index];
                if (value == -player) {
                    // Line contains both players and cannot be used.
                    player = BLANK;
                    break;
                }
                player = value;
                // Every blank space (including ones below the current space) decreases the score.
                for (rowTemp = row; (rowTemp >= 0) && (board->board[col][rowTemp] == BLANK); rowTemp--) {
                    movesAwayScore /= 7;
                }
            }
            score += player * movesAwayScore;
        }
    }


    //For every possible horizontal line.
    for(row = 0; row < HEIGHT; row++){
        for(col = 0;col < HEIGHT - 3; col++){
            // Multiplied by 7 for every extra move to get to the position.
            int movesAwayScore = 16807;
            Piece player = BLANK;
            // For every cell in the line.
            for (index = 0; index < 4; index++) {
                Piece value = board->board[col + index][row];
                if (value == -player) {
                    // Line contains both players and cannot be used.
                    player = BLANK;
                    break;
                }
                player = value;
                // Every blank space (including ones below the current space) decreases the score.
                for (rowTemp = row; (rowTemp >= 0) && (board->board[col][rowTemp] == BLANK); rowTemp--) {
                    movesAwayScore /= 7;
                }
            }
            score += player * movesAwayScore;
        }
    }

    //For every possible diagonal (bl->tr) line.
    for(row = 0; row < HEIGHT - 3; row++){
        for(col = 0;col < HEIGHT - 3; col++){
            // Multiplied by 7 for every extra move to get to the position.
            int movesAwayScore = 16807;
            Piece player = BLANK;
            // For every cell in the line.
            for (index = 0; index < 4; index++) {
                Piece value = board->board[col + index][row + index];
                if (value == -player) {
                    // Line contains both players and cannot be used.
                    player = BLANK;
                    break;
                }
                player = value;
                // Every blank space (including ones below the current space) decreases the score.
                for (rowTemp = row; (rowTemp >= 0) && (board->board[col][rowTemp] == BLANK); rowTemp--) {
                    movesAwayScore /= 7;
                }
            }
            score += player * movesAwayScore;
        }
    }

    //For every possible diagonal (tl->br) line.
    for(row = 2; row < HEIGHT; row++){
        for(col = 0;col < HEIGHT - 3; col++){
            // Multiplied by 7 for every extra move to get to the position.
            int movesAwayScore = 16807;
            Piece player = BLANK;
            // For every cell in the line.
            for (index = 0; index < 4; index++) {
                Piece value = board->board[col + index][row - index];
                if (value == -player) {
                    // Line contains both players and cannot be used.
                    player = BLANK;
                    break;
                }
                player = value;
                // Every blank space (including ones below the current space) decreases the score.
                for (rowTemp = row; (rowTemp >= 0) && (board->board[col][rowTemp] == BLANK); rowTemp--) {
                    movesAwayScore /= 7;
                }
            }
            score += player * movesAwayScore;
        }
    }

    return score;
}

// Implementation of the negamax search algorithm.
int negamax(Board* board, int depth, Piece player, int alpha, int beta, int lastCol) {
    int endGame = checkWin(board, lastCol, -player);
    if (endGame != 0) {
        return endGame;
    }
    if (depth == 0) {
        return player * eval(board);
    }
    int bestVal = INT_MIN;
    int val;
    // Centre pieces are more likely to be good.
    // col = 3, 4, 2, 5, 1, 6, 0.
    int col;
    for (col = WIDTH / 2; col < WIDTH; col = (col <= WIDTH / 2) ? WIDTH - col : WIDTH - col - 1) {
        if (addPiece(board, col, player)) {
            // Game hasn't ended.
            val = -negamax(board, depth - 1, -player, -beta, -alpha, col);
            if (val > bestVal) {
                bestVal = val;
                if (val > alpha) {
                    alpha = val;
                    if (alpha >= beta) {
                        removePiece(board, col);
                        break;
                    }
                } else {
                    fprintf(stderr, "val <= alpha???\n");
                }
            }
            removePiece(board, col);
        }
    }
    return bestVal;
}

int getBestMove(Board* board, int depth) {
    int alpha = INT_MIN;
    int beta = INT_MAX;

    int bestVal = INT_MIN;
    int bestCol = -1;
    int val;
    // Centre pieces are more likely to be good.
    // col = 3, 4, 2, 5, 1, 6, 0.
    int col;
    for (col = WIDTH / 2; col < WIDTH; col = (col <= WIDTH / 2) ? WIDTH - col : WIDTH - col - 1) {
        if (addPiece(board, col, ME)) {
            // Game hasn't ended.
            val = -negamax(board, depth - 1, YOU, -beta, -alpha, col);
            if (val > bestVal) {
                bestVal = val;
                bestCol = col;
                if (val > alpha) {
                    alpha = val;
                    if (alpha >= beta) {
                        removePiece(board, col);
                        break;
                    }
                } else {
                    fprintf(stderr, "val <= alpha?\n");
                }
            }
            removePiece(board, col);
        }
    }
    return bestCol;
}

// #include <stdlib.h>
// int main(int argv, char** argc) {
//     Board* board = calloc(sizeof(Board));
//     resetBoard(board);
//     addPiece(board, 3, ME);
//     removePiece(board, 3);
//     int bestMove = getBestMove(board, 2);
//     assert(bestMove == 3);
//     free(board);
// }
