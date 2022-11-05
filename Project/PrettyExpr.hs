module PrettyExpr (prettyLExp, printLExp,prettyLexpNewLine) where

import Expr

paren :: String -> String
paren s = "(" ++ s ++ ")"

parenIf :: Bool -> String -> String
parenIf b = if b then paren else id

prettyLexpNewLine :: String -> String
prettyLexpNewLine exp = exp ++ "\n"

prettyLExp :: LExp -> String
prettyLExp (V x) = x
prettyLExp (A t1 t2) = parenIf (isLam t1) (prettyLExp t1) ++ " " ++ parenIf (not $ isVar t2) (prettyLExp t2)
prettyLExp (L x t1) = "\\" ++ x ++ "." ++ prettyLExp t1

printLExp :: LExp -> IO ()
printLExp = putStrLn . prettyLExp
