#include <limits.h>
#include <stdbool.h>
#include <jni.h>

#define HEIGHT 6
#define WIDTH 7

typedef char Piece; // Use minimum bytes to store a piece.

#define ME ((Piece)1)
#define YOU ((Piece)-1)
#define BLANK ((Piece)0)

typedef struct {
    Piece board[WIDTH][HEIGHT]; // Representation of the board.
    int pieceHeight[WIDTH]; // Number of pieces in each column.
    int pieceCount; // Total number of pieces in use.
} Board;


int getBestMove(Board* board, int depth);

void resetBoard(Board* board);

// Adds a piece to the board. Returns false if this cannot be done.
bool addPiece(Board* board, int col, Piece piece);

// Removes the most recently added piece from the column.
void removePiece(Board* board, int col);

// Checks for a win. Returns INT_MAX for a win,
// INT_MIN + 1 if it is a draw and 0 if the game hasn't ended.
int checkWin(Board* board, int col, Piece lastPlayer);

JNIEXPORT jint JNICALL Java_neilw4_c4scala_controller_NativeAi_00024_nativeAdviseMove(JNIEnv* env, jobject this, jbyteArray jBoard, jintArray jHeights, jint jPieceCount, jint difficulty);
