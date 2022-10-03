import Control.Monad.State
import Control.Monad.Fail
import System.Random
import Data.List

data Expr = Con Double | Sub Expr Expr | Div Expr Expr
    deriving (Show,Eq)

e1 = Sub (Div (Con 2) (Con 4)) (Con 3)
e2 = Sub (Con 1) (Div (Con 2) (Con 2))
e3 = Div (Con 1) (Sub (Con 2) (Con 2))

-- Exercise 1a
evalSafe :: Expr -> Maybe Double
evalSafe (Con d) = do
    return d
evalSafe (Sub expr1 expr2) = do
    x1 <- evalSafe expr1
    x2 <- evalSafe expr2
    return (x1 - x2)
evalSafe (Div expr1 expr2) = do
    x1 <- evalSafe expr1
    x2 <- evalSafe expr2
    if x2 /= 0 then return (x1 / x2) else Nothing

-- Exercise 1b
evalSafeMF :: MonadFail m => Expr -> m Double
evalSafeMF (Con d) = do
    return d
evalSafeMF (Sub expr1 expr2) = do
    x1 <- evalSafeMF expr1
    x2 <- evalSafeMF expr2
    return (x1 - x2)
evalSafeMF (Div expr1 expr2) = do
    x1 <- evalSafeMF expr1
    x2 <- evalSafeMF expr2
    if x2 /= 0 then return (x1 / x2) else fail "Can't divide by zero"


{- different outputs of evalSafeMF ... -}

evalWeird :: Expr -> StateT Int Maybe Double
evalWeird (Con c)    =
  get >>= \n ->
  put (n+1) >>= \_ ->
  return (if n `mod` 3 == 2 then 0 else c)
evalWeird (Sub e1 e2) =
  evalWeird e1 >>= \x1 ->
  evalWeird e2 >>= \x2 ->
  return (x1-x2)
evalWeird (Div e1 e2) =
  evalWeird e1 >>= \x1 ->
  evalWeird e2 >>= \x2 ->
  if x2 /= 0 then return (x1/x2) else lift Nothing
evalWeirdTop e = runStateT (evalWeird e) 0 >>= \(x,s) -> return x

-- Exercise 1c
evalWeird' :: MonadFail m => Expr -> StateT Int m Double
evalWeird' (Con c) = do
    n <- get
    _ <- put(n+1)
    return (if n `mod` 3 == 2 then 0 else c)
evalWeird' (Sub e1 e2) = do
    x1 <- evalWeird' e1
    x2 <- evalWeird' e2
    return (x1 - x2)
evalWeird' (Div expr1 expr2) = do
    x1 <- evalWeird' e1
    x2 <- evalWeird' e2
    if x2 /= 0 then return (x1 / x2) else lift (fail "Can't divide by 0")

evalWeirdTop' :: MonadFail m => Expr -> m Double
evalWeirdTop' e = runStateT (evalWeird' e) 0 >>= \(x,s) -> return x


--________________________________________________________
--________________________________________________________

data Bin a = L a | B (Bin a) (Bin a)
  deriving (Show,Eq)

mapBin :: (a -> b) -> Bin a -> Bin b
mapBin f (L x)     = L (f x)
mapBin f (B tL tR) = B (mapBin f tL) (mapBin f tR)

instance Functor Bin where
  fmap = mapBin

-- Exercise 2a
{-
    Here the functor mapBin is a morphism for the Category Bin
    which can be represented as mapBin: Bin -> Bin
                    or
    in other words, if x \in Bin then we can represent the
    functor as mapBin: x -> Bin(x)
    For id func we have:
        mapBin id t = t

-}

-- Exercise 2b
instance Monad Bin where
  return = L
  (>>=) = undefined
--  L a >>= f = f a
--  B a b >>= f = B (f a) (f b)

instance Applicative Bin where
  pure = return
  xm <*> ym = xm >>= \x -> ym >>= return . x

-- Exercise 2c (optional)
{- Your proof goes here -}

-- Exercise 2d (optional)
{- Your thoughts go here -}

--________________________________________________________
--________________________________________________________

class Monad m => SelectMonad m where
  select :: [a] -> m a

instance SelectMonad [] where
  select = id

instance SelectMonad IO where
  select xs
    | not (null xs) = do i <- getStdRandom (randomR (0, length xs-1))
                         return (xs !! i)
    | otherwise     = fail "cannot select from empty list"

newtype Dist a = Dist { dist :: [(a,Rational)] }  deriving (Show)

instance Monad Dist where
  return x = Dist [(x,1)]
  xm >>= f = Dist [(y,p*q) | (x,p) <- dist xm, (y,q) <- dist (f x)]
  
-- We add the following standard boilerplate to derive instances of the
-- Functor and Applicative type classes, from the Monad instance above:
instance Functor Dist where
  fmap f xm = xm >>= return . f

instance Applicative Dist where
  pure = return
  xm <*> ym = xm >>= \x -> ym >>= return . x

instance SelectMonad Dist where
  select xs
    | not (null xs) = let n = length xs in Dist [(x, 1 / fromIntegral n) | x <- xs]
    | otherwise     = error "cannot select from empty list"

code :: SelectMonad m => m Char
code = do
  i <- select [0..3]
  return ("hello" !! i)

prob :: Eq a => Dist a -> a -> Rational
prob xm x = sum [p | (y,p) <- dist xm, x == y]

normalize :: Eq a => Dist a -> Dist a
normalize xm = Dist [(x,prob xm x) | x <- support xm]
  where
    support :: Eq a => Dist a -> [a]
    support xm = nub [x | (x,p) <- dist xm, p > 0]  -- "nub", defined in Data.List, removes duplicates

-- Exercise 3a
coin :: SelectMonad m => m Bool
coin = select [True, False]

-- Exercise 3b
subset :: SelectMonad m => [a] -> m [a]
subset []     = return []
subset (x:xs) = do
  c <- coin
  ys <- subset xs
  if c then return (x:ys)
  else return ys

-- Exercise 3c
simulate :: Monad m => Int -> m Bool -> m Int
simulate 0 b = return 0
simulate n b = do
  k <- simulate (n-1) b
  b' <- b
  if b' then return (k+1) else return k

-- Exercise 3d (optional)
genTree :: SelectMonad m => [a] -> m (Bin a)
genTree = undefined

