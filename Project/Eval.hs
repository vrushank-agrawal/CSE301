module Eval (normalize, normalizeWithSteps) where

import Expr
import Subst

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
redex expr = filter isRedex (subexp expr)
  where
    isRedex :: LExpPtr -> Bool
    isRedex (_, A (L _ _) _) = True
    isRedex _ = False

stepBeta :: LExp -> LExp
stepBeta expr = plug ctx (subst (d, x) y)
  where
    redices = redex expr
    leftmost = head redices
    (ctx, e) = leftmost
    A (L x y) d = e

normalize :: LExp -> LExp
--normalize = stepBeta

normalize expr = if null (redex expr) then expr else normalize (stepBeta expr)
--normalize expr = if null (redex expr) then rename (\x -> [head x]) expr else normalize (stepBeta expr)

normalizeWithSteps :: LExp -> [LExp]
normalizeWithSteps expr = if null (redex expr) then [expr] else expr : normalizeWithSteps (stepBeta expr)