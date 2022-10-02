--- Programming in untyped lambda calculus

-- Encodings of booleans and natural numbers from class
{-
true = \x.\y.x
false = \x.\y.y
not = \b.\x.\y.b y x
and = \b.\c.b c false
zero = \f.\x.x
one = \f.\x.f x
two = \f.\x.f (f x)
succ = \n.\f.\x.f (n f x)
add = \m.\n.m succ n
mult = \m.\n.m (add n) 0
isZero = \n.n (\b.false) true
-}

-- Exercise 1a
{-
isEven = \n.n not true
-}

-- Exercise 1b
{-
exp = \n.\m. m n
-}

-- Encodings of pairing and projections
{-
pair = \x.\y.\f.f x y
fst = \p.p (\x.\y.x)
snd = \p.p (\x.\y.y)
-}

-- Exercise 1c
{-
swap = \p.p (\x.\y.\f. f y x)
-}

-- Exercise 1d
{-
swapIf = \b.\p. p (\x.\y.\f. ((b (f y x)) (f x y)))
-}

-- Exercise 1e (optional)
{-
fib = <your definition here>
-}

-- Exercise 1e (optional)
{-
pred = \n.\f.\x. n (\g.\h. h (g f)) (\u.x) (\u.u)
-}

-- Curry's and Turing's fixed point operators
{-
Y = \x.(\y.x(y y))(\y.x (y y))
Theta = (\x.\y.y (x x y)) (\x.\y.y (x x y))
-}

-- Exercise 1f (optional)
{-
collatz = <your definition here>
-}

--- STLC and type inference

-- Exercise 2a
{-
e1 :: (a -> f) -> a -> f
e2 :: (f -> f) -> f -> f
e3 :: ((a -> a) -> f) -> f
e4 :: (a -> a -> f) -> a -> f
e5 :: function not possible because of infinite type
e6 :: (y -> x) -> ((y -> x) -> y) -> x
-}


-- Exercise 2b
fn1 :: a -> b -> (a -> b -> c) -> c
fn1 a b f = f a b
fn2 :: a -> b -> (a -> b -> c) -> (a,b,c)
fn2 a b f = (a, b, (f a b))
fn3 :: ([a] -> b) -> a -> b
fn3 f a = f [a]
fn4 :: ((a -> a) -> b) -> b
fn4 f = f fn5
fn5 :: a -> a
fn5 a = a


-- Exercise 2c (optional)
{-
mysterylam = Not possible because we are constructing an infinite type where
             a function takes itself as an argument and returns itself which
             doesn't make sense
-}


-- Exercise 2d (optional)
mysteryfn = undefined

--- Bidirectional typing

data Ty = TV Int | Fn Ty Ty
    deriving (Show,Eq)

data Expr = V Int | A Expr Expr | L Int Expr | Ann Expr Ty
    deriving (Show,Eq)

bcomp = L 0 $ L 1 $ L 2 $ A (V 0) (A (V 1) (V 2))

oneid = A (Ann (L 0 $ L 1 $ A (V 0) (V 1)) (Fn (Fn (TV 0) (TV 0)) (Fn (TV 0) (TV 0)))) (L 0 $ V 0)

type TyCxt = [(Int,Ty)]


-- Exercise 3
check :: TyCxt -> Expr -> Ty -> Bool
check = undefined
--check cxt expr ty = do
--        (ty', cxt') <- synth cxt expr
----        cxt'' ->
--        return cxt'

synth :: TyCxt -> Expr -> Maybe Ty
synth cxt expr@(V _) = Nothing              -- first case
--synth expr@(A e1 e2) = synth


