import Cmd
import Control.Monad (when)
import Data.List
import Eval
import Expr
import Parser
import PrettyExpr
import Subst
import System.IO

-- an "environment" is a list of variable names paired with their definitions as lambda-expressions
type Env = [(Var, LExp)]

-- undefinedVar determines whether an expression has any free variable that is not defined by the environment
undefinedVar :: Env -> LExp -> Maybe Var
undefinedVar env t = find (\y -> lookup y env == Nothing) (free t)

-- getKey :: IO [Char]
-- getKey = reverse <$> getKey' ""
--   where getKey' chars = do
--           char <- getChar
--           more <- hReady stdin
--           (if more then getKey' else return) (char:chars)

-- -- Simple menu controller
-- main = do
--   hSetBuffering stdin NoBuffering
--   hSetEcho stdin False
--   key <- getKey
--   when (key /= "\ESC") $ do
--     case key of
--       "\ESC[A" -> putStr "↑"
--       "\ESC[B" -> putStr "↓"
--       "\ESC[C" -> putStr "→"
--       "\ESC[D" -> putStr "←"
--       "\n"     -> putStr "⎆"
--       "\DEL"   -> putStr "⎋"
--       _        -> return ()
--     main

-- the top-level read-eval-print-loop
repl :: IO ()
repl = go [] -- start the interpreter in an empty environment
  where
    go :: Env -> IO ()
    go env = do
      putStr "> " -- print the prompt
      hFlush stdout -- flush standard output
      line <- getLine -- get a line of input
      let cmd = read line -- parse the input as a command
      case cmd of
        Eval t ->
          -- execute an eval command
          -- the expression to be evaluated cannot have any free
          -- variables that are not defined in the environment
          case undefinedVar env t of
            Just y -> putStrLn ("Variable not in scope: " ++ y) >> go env
            Nothing -> do
              -- substitute for all variables defined in the environment,
              -- in order from left to right
              let t' = foldl (\t (x, u) -> subst (u, x) t) t env
              -- normalize the resulting term
              let u = normalize t'
              --let u = normAll t'
              -- print the result
              --mapM_ (putStrLn . prettyLExp) u
              printLExp u
              -- continue the REPL
              go env
        Let x t ->
          -- execute a let command
          case undefinedVar env t of
            Just y -> putStrLn ("Variable not in scope: " ++ y) >> go env
            Nothing -> do
              -- continue the REPL in an environment extended with x=t
              go ((x, t) : env)
        Noop -> go env -- execute a no-op command, by doing nothing and continuing the REPL
        Quit -> do
          -- execute a quit command, by terminating the REPL
          putStrLn "Goodbye."
          return ()

main :: IO ()
main = repl
