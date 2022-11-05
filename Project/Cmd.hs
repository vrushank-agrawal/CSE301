module Cmd where

import Expr

data Cmd = Eval LExp | Let Var LExp | Noop | Quit | Load String
  deriving (Show, Read)
