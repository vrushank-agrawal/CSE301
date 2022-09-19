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
my_transpose = undefined

--- Folding exercises

-- Exercise 2a
altsum :: Num a => [a] -> a
altsum xs = let signs = cycle [1, -1]
            in foldl (+) 0 (zipWith (*) signs xs)

-- Exercise 2b
my_intersperse :: a -> [a] -> [a]
my_intersperse i xs = foldr (\x ys -> x : if null ys then [] else i:ys ) [] xs

-- Exercise 2c
my_tails :: [a] -> [[a]]
--my_tails xs = foldr (\x ys -> [x] : if null ys then [] else [x:ys] ) [] xs
my_tails = undefined

-- Exercise 2d (optional)
my_isPrefixOf :: Eq a => [a] -> [a] -> Bool
my_isPrefixOf = undefined

-- Exercise 2e (optional)
my_dropWhile :: (a -> Bool) -> [a] -> [a]
my_dropWhile = undefined

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
toDLrev = undefined

-- Exercise 3b
my_reverse :: [a] -> [a]
my_reverse = undefined

naive_reverse :: [a] -> [a]
naive_reverse []     = []
naive_reverse (x:xs) = naive_reverse xs ++ [x]

--- Regular expression matching

data RegExp = Zero | One
            | C Char
            | Plus RegExp RegExp | Times RegExp RegExp
            | Star RegExp
  deriving (Show,Eq)

accept :: RegExp -> String -> Bool

accept e w = acc e w null

-- Exercise 4a
acc :: RegExp -> String -> (String -> Bool) -> Bool
acc Zero          w k = False
acc One           w k = k w
acc (C a)         w k = undefined
acc (Plus e1 e2)  w k = undefined
acc (Times e1 e2) w k = undefined

-- Exercise 4b (optional)
acc (Star e)      w k = undefined
