module Eval (normalize, normAll) where

import Expr
import Subst
import System.IO
import System.Random (getStdRandom, newStdGen, random, randomR, randomRs)

-- datatype of one-hole contexts for lambda expressions
data LExpCxt = Hole | A'1 LExpCxt LExp | A'2 LExp LExpCxt | L' Var LExpCxt
  deriving (Show, Eq)

test1 = (L "x" (A (V "x") (V "y")))
-- we represent contexts "inside-out", i.e., with the parts of the
-- context nearest to the hole at the top-level.
-- The plugging function is defined accordingly.
plug :: LExpCxt -> LExp -> LExp
plug Hole d = d
plug (A'1 c e2) d = plug c (A d e2)
plug (A'2 e1 c) d = plug c (A e1 d)
plug (L' x c) d = plug c (L x d)

-- a pointer to a subexpression (a.k.a. "zipper") is a pair of a context and an expression
type LExpPtr = (LExpCxt, LExp)

-- a template for implementing normalize, as described in the
-- mini-project page...

subexp :: LExp -> [LExpPtr]
subexp t = focus t Hole
  where
    focus :: LExp -> LExpCxt -> [LExpPtr]
    focus (V x) c = [(c, V x)] -- base case return context of variable
    -- recursive case for each part of expressions of an application
    -- put the context of the hole in the list of subexpressions
    focus (A e1 e2) c = (c, A e1 e2) : focus e1 (A'1 c e2) ++ focus e2 (A'2 e1 c)
    -- recursive case for lambda expression
    focus (L x e) c = (c, L x e) : focus e (L' x c)

redex :: LExp -> [LExpPtr]
redex expr = filter isRedex (subexp expr)
  where
    isRedex :: LExpPtr -> Bool
    isRedex (_, A (L _ _) _) = True
    isRedex _ = False

stepBeta :: LExp -> String -> IO LExp
stepBeta expr eval = do
  leftmost <- picker redices eval
  case leftmost of
    (ctx, e) -> case e of
      A (L x y) d -> return $ plug ctx (subst (d, x) y)
  where
    redices = redex expr

picker :: [LExpPtr] -> String -> IO LExpPtr
picker redices s
  | s == "norm" = return (head redices)
  | s == "appl" = return (last redices)
  | s == "rand" = do
        x <- getStdRandom $ randomR (0, length redices - 1)
        return (redices !! x)

normalize :: LExp -> String -> IO [LExp]
normalize expr eval
  | null (redex expr) = return [expr]
  | otherwise = do
        next <- stepBeta expr eval
        rest <- normalize next eval
        return rest

normAll :: LExp -> String -> IO [LExp]
normAll expr eval
  | null (redex expr) = return [expr]
  | otherwise = do
        next <- stepBeta expr eval
        rest <- normAll next eval
        return $ expr : rest
