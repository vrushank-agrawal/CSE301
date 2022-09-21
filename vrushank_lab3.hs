import Data.List

--- Zipping exercises

-- Exercise 1a
my_zip :: [a] -> [b] -> [(a,b)]
my_zip = zipWith (,)

-- Exercise 1b
my_zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
my_zipWith f xs ys = map (uncurry f) (zip xs ys)

-- Exercise 1c (optional)
my_transpose :: [[a]] -> [[a]]
my_transpose [] = []
my_transpose ((x:xs):ns) = (x:heads'): transpose (xs:tails')
                           where (heads', tails') = unzip [(y, ys) | y:ys <- ns]

--- Folding exercises

-- Exercise 2a
altsum :: Num a => [a] -> a
altsum = foldr (\x y -> x - y) 0

-- More efficient higher order function implementation
--altsum xs = let signs = cycle [1, -1]
--            in foldl (+) 0 (zipWith (*) signs xs)

-- Exercise 2b
my_intersperse :: a -> [a] -> [a]
my_intersperse i xs = foldr (\x ys -> x : if null ys then [] else i:ys ) [] xs

-- Exercise 2c
my_tails :: [a] -> [[a]]
my_tails xs = foldr (\x (y:ys) -> (x:y):(y:ys)) [[]] xs

-- Exercise 2d (optional)
my_isPrefixOf :: Eq a => [a] -> [a] -> Bool
my_isPrefixOf = undefined

-- Exercise 2e (optional)
my_dropWhile :: (a -> Bool) -> [a] -> [a]
my_dropWhile f xs = foldr (\x ys bool -> if bool && (f x)
                                         then ys True
                                         else x:ys False) (const []) xs True

--- Difference lists

type DiffList a = [a] -> [a]

toDL :: [a] -> DiffList a
toDL xs = (xs++)

fromDL :: DiffList a -> [a]
fromDL dxs = dxs []

cons :: a -> DiffList a -> DiffList a
cons x dxs = (x:) . dxs

snoc :: DiffList a -> a -> DiffList a
snoc dxs x = dxs . (x:)

-- Exercise 3a
toDLrev :: [a] -> DiffList a
--toDLrev = foldr (\x xs -> ((toDLrev xs).(x:))) ([]++)
toDLrev [] = ([]++)
toDLrev (x:xs) = snoc (toDLrev xs) x

-- Exercise 3b
my_reverse :: [a] -> [a]
my_reverse x = (toDLrev x) []

naive_reverse :: [a] -> [a]
naive_reverse []     = []
naive_reverse (x:xs) = naive_reverse xs ++ [x]

--- Regular expression matching

data RegExp = Zero | One
            | C Char
            | Plus RegExp RegExp
            | Times RegExp RegExp
            | Star RegExp
  deriving (Show,Eq)

accept :: RegExp -> String -> Bool

accept e w = acc e w null

-- Exercise 4a
acc :: RegExp -> String -> (String -> Bool) -> Bool
acc Zero          w k = False
acc One           w k = k w
acc (C a)        "" k = False
acc (C a)         w k = if (head w) == a then k (tail w) else False
acc (Plus e1 e2)  w k
        | acc e1 w k = True
        | acc e2 w k = True
        | otherwise  = False
acc (Times e1 e2) "" k
        | acc e1 "" k = acc e2 "" k
        | otherwise   = False
acc (Times e1 e2) [x] k
        | acc e1 [x] k = acc e2 "" k
        | acc e2 [x] k = acc e1 "" k
        | otherwise  = False
acc (Times e1 e2) (x:xs) k
        | acc e1 [x] k = acc e2 xs k
        | otherwise  = False

-- Exercise 4b (optional)
acc (Star e)      w k = undefined
