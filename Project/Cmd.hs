module Cmd where

import Expr

data Cmd = Eval LExp | Let Var LExp | Noop | Quit | Load String | Set String
  deriving (Show, Read)
