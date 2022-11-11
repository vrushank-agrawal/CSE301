import Cmd
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.List
import Eval
import Expr
import Parser
import PrettyExpr
import Subst
import System.Exit
import System.IO

-- an "environment" is a list of variable names paired with their definitions as lambda-expressions
type Env = [(Var, LExp)]

-- a "flag" is a tuple of command flags paired with their setting - on or off - as a bool
type Flags = (Bool, String, Bool)

-- get the first flag
first :: Flags -> Bool
first (b, _, _) = b

-- get the second flag
second :: Flags -> String
second (_, s, _) = s

-- get the third flag
third :: Flags -> Bool
third (_, _, b) = b

-- undefinedVar determines whether an expression has any free variable that is not defined by the environment
undefinedVar :: Env -> LExp -> Maybe Var
undefinedVar env t = find (\y -> lookup y env == Nothing) (free t)

-- the top-level read-eval-print-loop
repl :: IO ()
repl = go [] (False, "norm", False) -- start the interpreter in an empty environment
  where
    go :: Env -> Flags -> IO ()
    go env flags = do
      putStr "> " -- print the prompt
      hFlush stdout -- flush standard output
      line <- getLine -- get a line of input
      cmd <- readParser parseCmd line -- parse the input as a command
      case cmd of
        Eval t ->
          -- execute an eval command
          -- the expression to be evaluated cannot have any free
          -- variables that are not defined in the environment
          case undefinedVar env t of
            Just y -> putStrLn ("Variable not in scope: " ++ y) >> go env flags
            Nothing -> do
              -- substitute for all variables defined in the environment,
              -- in order from left to right
              let t' = foldl (\t (x, u) -> subst (u, x) t) t env
              -- normalize the resulting term
              u <- (if first flags then normAll else normalize) t' $ second flags
              -- print the result
              printLExp (third flags) u
              -- continue the REPL
              go env flags
        Let x t ->
          -- execute a let command
          case undefinedVar env t of
            Just y -> putStrLn ("Variable not in scope: " ++ y) >> go env flags
            Nothing -> do
              -- continue the REPL in an environment extended with x=t
              go ((x, t) : env) flags
        Noop -> go env flags -- execute a no-op command, by doing nothing and continuing the REPL
        Quit -> do
          -- execute a quit command, by terminating the REPL
          putStrLn "Goodbye."
          return ()
        Load name -> do
          (env, flags) <- liftIO $ envFromFile name env flags --read file and convert back from IO
          putStrLn "File loaded!"
          go env flags
        Set set -> do
          -- change flags
          let action
                | set == "stepon" = go env (True, second flags, third flags)
                | set == "stepoff" = go env (False, second flags, third flags)
                | set == "normal" = go env (first flags, "norm", third flags)
                | set == "applicative" = go env (first flags, "appl", third flags)
                | set == "random" = go env (first flags, "rand", third flags)
                | set == "original" = go env (first flags, second flags, False)
                | set == "haskell" = go env (first flags, second flags, True)
                | otherwise = go env flags
          action

-- Function that reads definitions from a file and stores them as env variables
envFromFile :: FilePath -> Env -> Flags -> IO (Env, Flags)
envFromFile name env flags = do
  content <- readFile name
  let content_lines = lines content
  content_to_env content_lines env 0 flags
  where
    content_to_env :: [String] -> Env -> Int -> Flags -> IO (Env, Flags)
    content_to_env [] env _ flags = return (env, flags) --not an action
    content_to_env (x : xs) env n flags = do
      putStrLn ("> " ++ x)
      res <- readParser parseCmd x
      case res of
        Let x t ->
          -- execute a variable definition
          case undefinedVar env t of
            Just y -> do
              putStrLn ("Variable not in scope: " ++ y)
              content_to_env xs env n flags -- continue to next lines
            Nothing -> do
              -- continue the REPL in an environment extended with x=t
              content_to_env xs ((x, t) : env) (n + 1) flags --add to environment
        Eval t -> do
          case undefinedVar env t of
            Just y -> putStrLn ("Variable not in scope: " ++ y)
            Nothing -> do
              let t' = foldl (\t (x, u) -> subst (u, x) t) t env
              u <- if first flags then normAll t' $ second flags else normalize t' $ second flags
              printLExp (third flags) u
          content_to_env xs env n flags -- continue to next lines
        Noop -> do
          content_to_env xs env n flags -- continue to next lines
        Quit -> do
          putStrLn "Goodbye."
          exitSuccess
        Set set -> do
          let action
                | set == "stepon" = content_to_env xs env n (True, second flags, third flags)
                | set == "stepoff" = content_to_env xs env n (False, second flags, third flags)
                | set == "normal" = content_to_env xs env n (first flags, "norm", third flags)
                | set == "applicative" = content_to_env xs env n (first flags, "appl", third flags)
                | set == "random" = content_to_env xs env n (first flags, "rand", third flags)
                | set == "original" = content_to_env xs env n (first flags, second flags, False)
                | set == "haskell" = content_to_env xs env n (first flags, second flags, True)
                | otherwise = content_to_env xs env n flags
          action
        _ -> do
          putStrLn "Command not recognized by file parsing"
          return (env, flags)

main :: IO ()
main = repl
