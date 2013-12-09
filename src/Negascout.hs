-- | Negascout and other (mostly alpha-beta pruning) algorithms for game-tree search
-- Copyright 2009 Colin Adams
--
-- This file is part of game-tree.
--
--  Game-tree is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.

--  Game-tree is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.

--  You should have received a copy of the GNU General Public License
--  along with game-tree.  If not, see <http://www.gnu.org/licenses/>.

module Data.Tree.Game_tree.Negascout (
                                 -- * Access
                                 negamax,
                                 alpha_beta_search,
                                 principal_variation_search,
                                 negascout
                 ) where

import Data.Tree.Game_tree.Game_tree

{-# contract negascout :: Ok -> {depth | depth >= 0} -> {(rs, rv) | not null rs} #-}
-- |  Negascout alpha-beta pruning algorithm.
--
-- Node_value needs to be sensitive to whose turn it is to move.
-- I.e. it must return values of the opposite sign if the other player is to move.
negascout :: Game_tree a => a   -- ^ State to be evaluated
          -> Int          -- ^ Search this deep
          -> ([a], Int)   -- ^ (Principal variation, Score)
negascout node depth = negascout'  ((minBound :: Int) + 1) (maxBound :: Int) node depth

{-# contract principal_variation_search :: Ok -> {depth | depth >= 0} -> {(rs, rv) | not null rs} #-}
-- | Alpha-beta pruning with null-window search around every move after a move 
-- that improves alpha has been found
principal_variation_search :: Game_tree a => a   -- ^ State to be evaluated
                           -> Int          -- ^ Search this deep
                           -> ([a], Int)   -- ^ (Principal variation, Score)
principal_variation_search node depth
    | is_terminal node || depth == 0 = ([node], node_value node)
    | otherwise                      = case pvs ((minBound :: Int) + 1) (maxBound :: Int) (children node) depth of
                                         (pvm, pvv) -> (node:pvm, pvv)

{-# contract alpha_beta_search ::  Ok -> {depth | depth >= 0} -> {(rs, rv) | not null rs} #-}
-- | Normal alpha beta pruning (no window).
alpha_beta_search :: Game_tree a => a -- ^ State to be evaluated
           -> Int               -- ^ Search this deep
           -> ([a], Int)        -- ^ (Principal variation, Score)
alpha_beta_search node depth =
    alpha_beta ((minBound :: Int) + 1) (maxBound :: Int) node depth

{-# contract negamax :: Ok -> {depth | depth >= 0} -> {(rs, rv) | not null rs} #-}
-- | Plain negamax (= minimax with negative scores at alternate levels).
--  No alpha-beta pruning.
negamax :: Game_tree a => a  -- ^ State to be evaluated
        -> Int         -- ^ Search this deep
        -> ([a], Int)  -- ^ (Principal variation, Score)
negamax node depth
    | is_terminal node || depth == 0 = ([node], node_value node)
    | otherwise                      =  case children node of
                                          (c:cs) -> (node:pvm, pvv)
                                              where (pvm, pvv) = negaLevel (neg (negamax c (depth - 1))) cs
    where negaLevel prev_best@(_, old_v) (n:nn) =
              negaLevel best4 nn
                  where best4 = case neg $ negamax n (depth - 1) of 
                                  value@(_, v) | v > old_v -> value
                                               | otherwise -> prev_best
          negaLevel best _ = best                                 
          neg (m, v) = (m, -v)

-- Implementation

{-# contract negascout' :: Ok -> Ok -> Ok -> {depth | depth >= 0} -> {(rs, rv) | not null rs} #-}
-- |  Negascout alpha-beta pruning algorithm.
negascout' :: Game_tree a => Int -- ^ Minimum score maximising player is assured
          -> Int           -- ^ Maximum score minimizing player is assured
          -> a             -- ^ State to be evaluated
          -> Int           -- ^ Search this deep
          -> ([a], Int)    -- ^ (Principal variation, Score)
negascout'  alpha beta node depth
    | is_terminal node || depth == 0 = ([node], node_value node)
    | otherwise                      = let (pvm, pvv) = negascout'' [] alpha beta $ children node
                                       in (node:pvm, pvv)
    where
      d = depth - 1
      negascout'' npv nalpha _ [] = (npv, nalpha)
      negascout'' npv nalpha b (c:cs) = result
          where (n', alpha') = let (new_n', new_alpha') = negascout' (-b) (-nalpha) c d
                               in if (-new_alpha') > nalpha
                                  then (new_n', -new_alpha')
                                  else (npv, nalpha)
                result
                    | alpha' >= beta = ((c:n'), alpha')                                 -- beta cut-off
                    | alpha' >= b    = result'                                          -- check if null-window failed high
                    | otherwise      = negascout'' n' alpha' (alpha' + 1) cs            -- new null-window
                    where
                      result'
    -- CAUTION! Tracing shows the next line is not exercised by the unit tests.
                          | alpha'' >= beta = ((c:n''), alpha'')                        -- beta cut-off
                          | otherwise       = negascout'' n'' alpha'' (alpha'' + 1) cs
                          where
                            alpha'' = - alpha'''
                            (n'', alpha''') = negascout' (-beta) (-alpha') c d          -- full re-search

{-# contract pvs :: Ok -> Ok -> NotNull -> {depth | depth >= 0} -> {(rs, rv) | not null rs} #-}
-- | Principle variation search internals (soft fail).
--
-- The search continues as long as alpha < pvs < beta.
-- As soon pvs hits one these bounds the search stops and returns best.
pvs :: Game_tree a => Int -> Int -> [a] -> Int -> ([a], Int)
pvs alpha beta (c:cs) depth = case negpvs (-beta) (-alpha) c d of
                                best -> negaLevel best alpha beta cs
    where d = depth - 1
          negaLevel prev_best@(_, old_v) prev_alpha beta' (n:nn) | old_v < beta'
            = negaLevel best4 alpha' beta' nn
                where best4 = case negpvs (-alpha' - 1) (-alpha') n d of 
                                 value@(_, v) | (alpha' < v) && (v < beta')
                                                  -> negpvs (-beta') (-v) n d -- re-search
                                              | (v > old_v) -> value
                                              | otherwise -> prev_best 
                      alpha' = if old_v > prev_alpha then old_v else prev_alpha
          negaLevel best aa bb _     = best                                 
          negpvs alpha'' beta'' node d'
              | is_terminal node || d' == 0 = ([node], - (node_value node))
              | otherwise = case children node of
                              nn' -> (node:pvm, -pvv)
                                  where (pvm, pvv) = pvs alpha'' beta'' nn' d'

{-# contract alpha_beta :: Ok -> Ok -> Ok -> {depth | depth >= 0} -> {(rs, rv) | not null rs} #-}
-- | Normal alpha beta pruning (no window).
alpha_beta :: Game_tree a => Int -- ^ Minimum score maximising player is assured
           -> Int          -- ^ Maximum score minimizing player is assured
           -> a            -- ^ State to be evaluated
           -> Int          -- ^ Search this deep
           -> ([a], Int)   -- ^ (Principal variation, Score)
alpha_beta alpha beta node depth
    | is_terminal node || depth == 0 = ([node], node_value node)
    | otherwise                      =  case children node of
                                          (c:cs) -> (node:pvm, pvv)
                                              where (pvm, pvv) = negaLevel ([], (minBound :: Int) + 2) alpha beta (c:cs)
    where negaLevel prev_best@(_, old_v) prev_alpha beta' (n:nn) | old_v < beta'
            = negaLevel best4 alpha' beta' nn
                where best4 = case neg $ alpha_beta (-beta') (-alpha') n (depth - 1) of 
                                 value@(_, v) | (v > old_v) -> value
                                              | otherwise -> prev_best
                      alpha' = if old_v > prev_alpha then old_v else prev_alpha
          negaLevel best _ _ _     = best                                 
          neg (m, v) = (m, -v)


