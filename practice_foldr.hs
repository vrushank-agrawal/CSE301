--  Using the higher-order function foldr define a function sumsq which takes an
--  integer n as its argument and returns the sum of the squares of the first n
--  integers. That is to say,
--      sumsq n = 1^2 + 2^2 + 3^2 + . . . + n^2
--
--  Do not use the function map

sumsq :: Enum a => Num a => a -> a
sumsq n = foldr (\x y -> x^2 + y) 0 [1..n]


-- Define length, which returns the number of elements in a list, using foldr
lengt :: Num b => [a] -> b
lengt = foldr (\y ys -> 1 + ys) 0


-- Define reverse, which reverses a list, using foldr
rev :: [a] -> [a]
rev = foldr (\y ys -> ys ++ [y]) []


--  Using foldr , define a function remove which takes two strings as its arguments
--  and removes every letter from the second list that occurs in the first list. For
--  example, remove "first" "second" = "econd".
check :: (Eq a) => a -> [a] -> Bool
check _ []  =  False
check x (y:ys) | x == y    = True
               | otherwise = check x ys

remove :: (Eq a) => [a] -> [a] -> [a]
remove s1 = foldr (\x xs -> (if (check x s1) then [] else [x]) ++ xs) []


--  Define filter using foldr
filt :: (a -> Bool) -> [a] -> [a]
filt f = foldr (\y ys -> (if f y then [y] else []) ++ ys) []


--  The function remdups removes adjacent duplicates from a list. For example,
--          remdups [1, 2, 2, 3, 3, 3, 1, 1] = [1, 2, 3, 1].
--  Define remdups using foldr
remdups :: (Eq a) => [a] -> [a]
remdups = foldr (\y ys -> if null ys then y:ys else if (y == head ys) then ys else y:ys) []


--  The function inits returns the list of all initial segments of a list. Thus, inits
--  "ate" = [[], "a", "at", "ate"]. Define inits using foldr
initss :: [a] -> [[a]]
initss = foldr ((([] :) .) . map . (:)) [[]]