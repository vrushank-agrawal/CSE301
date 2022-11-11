module Eval (normalize, normAll) where

import Expr
import Subst
import System.Random (getStdRandom, randomR)

-- datatype of one-hole contexts for lambda expressions
data LExpCxt = Hole | A'1 LExpCxt LExp | A'2 LExp LExpCxt | L' Var LExpCxt
  deriving (Show, Eq)

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
redex expr = filter isRedex (subexp expr) -- filter by expressions that can be reduced
  where
    isRedex :: LExpPtr -> Bool
    -- Only expressions that are applications of some other expressions
    -- into a lambda expression can be reduced
    isRedex (_, A (L _ _) _) = True
    isRedex _ = False

stepBeta :: LExp -> String -> IO LExp
stepBeta expr eval = do
  -- select the evaluation strategy
  leftmost <- picker redices eval
  case leftmost of
    (ctx, e) -> case e of
      -- Do substitution and plug the expression into the original context
      A (L x y) d -> return $ plug ctx (subst (d, x) y)
  where
    -- expressions that can be reduced
    redices = redex expr

picker :: [LExpPtr] -> String -> IO LExpPtr
picker redices s
  -- use a flag to select the evaluation strategy
  | s == "norm" = return (head redices)
  | s == "appl" = return (last redices)
  | s == "rand" = do
    -- return a random element from the list
    x <- getStdRandom $ randomR (0, length redices - 1)
    return (redices !! x)

normalize :: LExp -> String -> IO [LExp]
normalize expr eval
  -- if there are no redices just return the expression
  | null (redex expr) = return [expr]
  -- otherwise reduce, and normalize the result
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
