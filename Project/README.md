# CSE301 Project

## An Interpreter for Untyped Lambda Calculus

_By Vrushank Agrawal, Yi Yao Tan and Lasha Koroshinadze_

The project supports following:

Flags can be set using `:s flagname` or `:set flagname` commands.

* Showing reduction steps:
* * `:s stepon` to show steps and `:s stepoff` to hide steps _(default)_
* Evaluation strategies:
* * `:s normal` _(default)_, `:s applicative` and `:s random`
* * Applicative order performs slower than normal for larger expressions
* * For example: `exp four five` takes `3.5` seconds on normal order compared to `4.3` seconds for applicative order on average.
* Syntax extensions:
* * `:s original` _(default view)_, `:s haskell` Haskell style expressions
* Loading a file:
* * `:l filename` or `:load filename`
