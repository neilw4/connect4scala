module C4Scala.Eval where

import C4Scala.Define
import C4Scala.Generic


type EvalFn = Board -> Score

evalFns width = [ evalH (width - 3)
                , evalV width
                , evalD1 (width - 3)
                , evalD2 (width - 3)
                ]

eval :: Board -> Int -> Score
eval board width = sum $ map (\f -> f board) (evalFns width)

score :: [Piece] -> Score
score pieces | length pieces < 4  = 0
             | and [a > 0, c > 0] = 0
             | a > 0              = negate 7 ^ a
             | c > 0              = 7 ^ c
         where (a, b, c) = countPieces pieces

countPieces :: [Piece] -> (Int, Int, Int)
countPieces [] _   = (0, 0, 0)
countPieces (x:xs) = case x of
                      -1 -> (a + 1, b, c)
                      0  -> (a, b + 1, c)
                      1  -> (a, b, c + 1)
          where (a, b, c) = countPieces xs
{-
score :: [Piece] -> Piece -> Score
score [] _ = 1
score (piece:pieces) match | piece == match = 7 * score pieces match
                           | otherwise      = 0
-}
evalV :: ColNo -> EvalFn
evalV 0 _ = 0
evalV count (col:cols) = evalV' (col:cols) + evalV (count - 1) cols

evalV' :: Board -> Score
evalV' ([]:cols) = 0
evalV' ((piece:pieces):cols) | piece     = evalV' (pieces:cols) + score pieces piece
                             | otherwise = evalV' (pieces:cols) - score pieces piece

evalH :: ColNo -> EvalFn
evalH 0 _ = 0
evalH count (col:cols) = evalH' (col:cols) + evalH (count - 1) cols

--TODO
evalH' :: Board -> Score
evalH' _ = 0

evalD1 :: ColNo -> EvalFn
evalD1 _ _ = 0

evalD2 :: ColNo -> EvalFn
evalD2 _ _ = 0
