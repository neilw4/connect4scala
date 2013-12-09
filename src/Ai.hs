module C4Scala.Ai where


import C4Scala.Define
import C4Scala.Eval
import C4Scala.Generic

import Data.Tree.Game_tree.Game_tree
import Data.Tree.Game_tree.Negascout

import Data.Maybe (fromJust, mapMaybe)
import Data.List (elemIndex)

depth = 5

solve :: C4Node -> ColNo
solve node = fromJust $ fromJust (bestPlay `elemIndex` children node) `elemIndex` colSeq (width node)
              where bestPlay = head.fst $ negascout node depth  -- Possibly need to get (!!2), not (head == (!!1))

instance Game_tree C4Node where
  is_terminal node | count node == 0                        = False
                   | count node == width node * height node = True
                   | otherwise                              = False  -- TODO: check for endgame

  node_value node = eval (board node) (width node)

  children node = mapMaybe (add node) $ colSeq $ width node

add :: C4Node -> ColNo -> Maybe C4Node
add (C4Node board piece count _ width height) col = maybeIf
                                          (C4Node (updateEntryAt (append piece) col board) (negate piece) (count + 1) col width height)
                                          $ length (board !! col) < width
