-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Lab6 where

import Data.List
import Data.Tree
import System.Random

import Types
--import DomViz
{- Uncomment the previous line if you want to use the visualization routines. -}

board4x4_3 = Board { turn = H,
                     free = [(1,1),(1,2),(2,2),(2,3),(2,4),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)],
                     hist = [(1,3),(2,1)] }

-- given a cell c and a player p, compute the adjacent cell c'
-- that is also occupied if p plays a domino at c
adjCell :: Cell -> Player -> Cell
adjCell (x,y) H = (x+1,y)
adjCell (x,y) V = (x,y+1)

-- compute the opponent of a player
opp :: Player -> Player
opp H = V
opp V = H

-- determine whether a move is valid in a given board
valid :: Board -> Cell -> Bool
valid b c = c `elem` free b && adjCell c (turn b) `elem` free b

-- create an empty board from an arbitrary list of cells
empty :: [Cell] -> Board
empty cs = Board { turn = H, free = cs, hist = [] }

-- create a rectangular board of arbitrary dimensions
board :: Int -> Int -> Board
board maxx maxy = empty [(x,y) | x <- [1..maxx], y <- [1..maxy]]

-- create a crosshatch-shaped square board of arbitrary dimension
hatch :: Int -> Board
hatch n = empty [(x,y) | x <- [1..2*n+1], y <- [1..2*n+1], odd y || x == 1 || x == (2*n+1) || odd x]

alphaDom_vs_LeeSedom =
  Board { turn = V,
          free = [(-4,1),(-4,3),(-2,0),(-2,4),(2,1),(2,4),(3,-4),(3,4),(4,-2),(4,0)],
          hist = [(0,4),(4,1),(0,-4),(-4,-3),(-1,-2),(2,-1),(-2,-4),(-4,-1),(-1,2),(4,3),(1,2),(-2,2),(-4,-4),(-2,-2),(2,-2),(4,-4),(-3,1),(2,-4),(-4,4),(-1,3),(-4,2),(-3,-2),(3,-1),(1,-3),(-2,-3),(3,1),(1,3)] }

alphaDom_vs_RanDom =
  Board { turn = V,
          free = [(-4,-3),(-4,0),(-2,-4),(-2,-2),(-1,-4),(-1,-2),(-1,2),(-1,4),(0,-4),(0,-2),(0,2),(0,4),(1,-4),(1,-2),(1,2),(1,4),(2,-4),(2,-2),(2,4),(3,-4),(4,0),(4,3)],
          hist = [(-3,4),(2,-1),(-3,2),(4,-2),(-4,-4),(-4,3),(3,4),(2,1),(-3,1),(3,1),(-4,-1),(-2,-1),(-2,3),(-4,1),(1,3),(4,-4),(-4,-2),(4,1),(1,-3),(3,-2),(-2,-3)] }


---------------------------------------------------------------------------------------------
-- Exercise 1
---------------------------------------------------------------------------------------------

-- Exercise 1a
--  returns the list of all legal moves for a player on a given board.
--  Note that here we mean the legal moves for either player, regardless
--  of whether it is that player's turn to play.

legalMoves :: Player -> Board -> [Cell]
legalMoves p b = foldr (\c cs -> (if adjCell c p `elem` (free b) then [c] else []) ++ cs) [] (free b)

-- Exercise 1b
--  takes a board and a legal move for the player whose turn it is to play,
--  and returns the new board resulting from executing that play. If the move
--  is actually illegal for the current player, then the behavior of moveLegal is unspecified

delElem :: [Cell] -> [Cell] -> Cell -> Cell -> [Cell]
delElem xs [] _ _ = xs
delElem cs (x:xs) c1 c2 | x == c1     = delElem cs xs c1 c2
                        | x == c2     = delElem cs xs c1 c2
                        | otherwise   = delElem (cs++[x]) xs c1 c2

moveLegal :: Board -> Cell -> Board
moveLegal b c = Board (opp (turn b)) (delElem [] (free b) c (adjCell c (turn b))) (c:(hist b))

-- Exercise 1c

prevState :: Board -> Cell -> Board
prevState b c = Board (opp (turn b)) (c:(free b)) (tail (hist b))

replay :: Board -> [Board]
replay b = foldr (\hc hcs -> (if null hcs then [] else [prevState (head hcs) hc]) ++ hcs) [b] (hist b)


---------------------------------------------------------------------------------------------
-- Exercise 2
---------------------------------------------------------------------------------------------

gametree :: Board -> Tree Board
gametree b = Node b [gametree (moveLegal b c) | c <- legalMoves (turn b) b]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

-- Exercise 2a
--  Implement the following heuristic: if it is p's turn to play, return a win for p's
--  opponent if p has no legal moves, and otherwise return a heuristic value based on the formula
--  #(legal moves for V) - #(legal moves for H) - sign(p)
--  where sign(H) = -1 and sign(V) = 1.

legalMovesNum :: [Cell] -> Int
legalMovesNum [] = 0
legalMovesNum (x:xs) = 1 + legalMovesNum xs

heuristic :: Player -> Int -> Int -> Score
heuristic V mV mH = Heu (mV - mH - 1)
heuristic H mV mH = Heu (mV - mH + 1)

score :: Board -> Score
score b | currentPlayerMoves == 0 = Win (opp (turn b))
        | otherwise               = heuristic (turn b) legalV legalH
        where legalH = legalMovesNum (legalMoves H b)
              legalV = legalMovesNum (legalMoves V b)
              currentPlayerMoves = legalMovesNum (legalMoves (turn b) b)

-- Exercise 2b
--  Annotate every node of a game tree with a minimax score, computed relative
--  to an arbitrary scoring function for the leaves. It should leave the tree
--  untouched other than annotating each node with a score: in other words, it
--  should satisfy that fmap fst (minimax sfn t) = t where the operation
--  fmap :: (a -> b) -> Tree a -> Tree b is defined in the Functor instance of Tree.


minimax :: (Board -> Score) -> Tree Board -> Tree (Board, Score)
minimax score tb = undefined

-- Exercise 2c
bestmoves :: Int -> (Board -> Score) -> Board -> [Cell]
bestmoves = undefined


---------------------------------------------------------------------------------------------
-- Exercise 3
---------------------------------------------------------------------------------------------

selectSafe :: SelectMonad m => [a] -> m (Maybe a)
selectSafe [] = return Nothing
selectSafe xs = select xs >>= \x -> return (Just x)
   
randomBestPlay :: SelectMonad m => Int -> (Board -> Score) -> Board -> m (Maybe Cell)
randomBestPlay d sfn = selectSafe . bestmoves d sfn

randomPlay :: SelectMonad m => Board -> m (Maybe Cell)
randomPlay b = selectSafe (legalMoves (turn b) b)

-- Exercise 3a
runGame :: SelectMonad m => (Board -> m (Maybe Cell)) -> (Board -> m (Maybe Cell)) -> Board -> m Board
runGame = undefined

-- Exercise 3b (optional)
carpets :: [Board]
carpets = undefined

-- alpha-beta pruning (optional)

