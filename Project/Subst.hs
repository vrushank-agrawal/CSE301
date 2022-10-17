module Subst (subst) where

import Expr

subst :: (LExp,Var) -> LExp -> LExp
-- definition for variable substitution
subst (d,x) (V v) | v == x     = d
                  | otherwise  = V v
-- definition for expression composition substitution
subst sub (A e1 e2) = A (subst sub e1) (subst sub e2)
-- definition for Lambda expression substitution
subst (d,x) (L v e) | v == x    = (L v e)
                    | otherwise = (L v (subst (d,x) e))