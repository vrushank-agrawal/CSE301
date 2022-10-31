module Subst (subst) where

import Expr

checkFree :: Var -> LExp -> Bool
checkFree v (V v'')   = v == v''
checkFree v (L e1 e2) = undefined

subst :: (LExp,Var) -> LExp -> LExp
-- definition for variable substitution
subst (d,x) (V v) | v == x     = d
                  | otherwise  = V v
-- definition for expression composition substitution
subst sub (A e1 e2) = A (subst sub e1) (subst sub e2)
-- definition for Lambda expression substitution
subst (d,x) (L v e) | v == x        = (L v e)
                    | checkFree v e = (L v (subst (d,x) e))