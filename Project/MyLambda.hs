import Cmd
import Control.Monad (when)
import Data.List
import Eval
import Expr
import Parser
import PrettyExpr
import Subst
import System.IO
import Control.Monad.IO.Class

-- an "environment" is a list of variable names paired with their definitions as lambda-expressions
type Env = [(Var, LExp)]

type Flags = (Bool, String)

-- undefinedVar determines whether an expression has any free variable that is not defined by the environment
undefinedVar :: Env -> LExp -> Maybe Var
undefinedVar env t = find (\y -> lookup y env == Nothing) (free t)

-- the top-level read-eval-print-loop
repl :: IO ()
repl = go [] (False, "norm") -- start the interpreter in an empty environment
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
              u <- if fst flags then normAll t' $ snd flags else normalize t' $ snd flags
              -- print the result
              mapM_ (putStrLn . prettyLExp) u
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
        Set set -> do
          let action
                | set == "stepon" = go env (True, snd flags)
                | set == "stepoff" = go env (False, snd flags)
                | set == "normal" = go env (fst flags, "norm")
                | set == "applicative" = go env (fst flags, "appl")
                | set == "random" = go env (fst flags, "rand")
                | otherwise = go env flags
          action
        Load name -> do
          env <- liftIO $ (envFromFile name env) --read file and convert back from IO
          putStrLn "Loaded!"
          go env flags
    
envFromFile :: FilePath -> Env -> IO Env
envFromFile name env = do 
                        content <- readFile name
                        let content_lines = lines content
                        (content_to_env content_lines env 0)
  where
  content_to_env :: [String] -> Env -> Int -> IO Env
  content_to_env [] env n = return env --not an action
  content_to_env (x: xs) env n = do 
                            putStrLn ("line: " ++ x)
                            res <- (readParser parseCmd x)
                            case res of
                              Let x t ->
                                  -- execute a let command
                                  case undefinedVar env t of
                                    Just y -> do
                                              putStrLn ("Variable not in scope: " ++ y) 
                                              return []
                                    Nothing -> do
                                      -- continue the REPL in an environment extended with x=t
                                    content_to_env xs ((x,t):env) (n+1) --add to environment
                              otherwise -> do 
                                            putStrLn ("Line " ++ (show n) ++ "is not a variable definition. Stopped at line before.")
                                            return [] --stop at error
                  
main :: IO ()
main = repl
