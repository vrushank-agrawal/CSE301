module Subst (subst) where

import Expr

-- function to replace variable in LExp
swap :: LExp -> Var -> Var -> LExp
swap (V v) y z
  | v == y = V z
  | otherwise = V v
swap (A e1 e2) y z = A (swap e1 y z) (swap e2 y z)
swap (L v e) y z
  | v == y = L z (swap e y z)
  | otherwise = L v (swap e y z)

-- function to modify variable name
modify :: Var -> Var
modify v = v ++ "'"

subst :: (LExp, Var) -> LExp -> LExp
-- definition for variable substitution
subst (d, x) (V v)
  | v == x = d
  | otherwise = V v
-- definition for expression composition substitution
subst dx (A e1 e2) = A (subst dx e1) (subst dx e2)
-- definition for Lambda expression substitution
subst (d, x) (L v e)
  | v == x = L v e
  | not (elem v (free d)) = L v (subst (d, x) e)
  | otherwise = L v' (subst (d, x) (swap e v v'))
  where
    v' = modify v
