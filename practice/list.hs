-- Getting last element of a list
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs
myLast [] = error "Can't get last of an empty list"

--Getting second last element of a list
secLast :: [a] -> a
secLast [a] = error "List is not long enough"
secLast [] = error "List is not long enough"
secLast (x:xs) = if length xs == 1
                 then x
                 else secLast xs

--Getting kth element of list
elementAt :: (Ord a, Eq a, Num a) => [a] -> a -> a
elementAt [] _ = error "Empty list"
elementAt (x:xs) a = if a < 1
                     then error "Invalid position"
                     else
                     if a == 1
                     then x
                     else elementAt xs (a-1)

--Reverse a list
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

--Check Palindrome
--isPalin :: [a] -> Bool
--isPalin [] = True
--isPalin [a] = True
--isPalin (x:xs) = if x == myLast xs
--                 then isPalin init xs
--                 else False

--Flatten a list
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

--compress a string with repeated characters
compress :: [Char] -> [Char]
compress [x] = [x]
compress (x:xs@(y:_))
      | x == y      = compress xs
      | otherwise   = x : compress xs

--pack similar elements of a list together
--pack :: [Char] -> [Char]
--pack [] = []
--pack (x:xs@(y:ys))
--      | x == y      = pack [x:' ':y:[]] ++ pack ys
--      | otherwise   = pack [x] ++ pack xs

----Encode
--encode :: Eq a => [a] -> [(Int, a)] -> [(Int, a)]
--encode [] xs = xs
--encode (x:xs) [] = encode xs [(1, x)]
--encode (x:xs) [(a, b)]
--      | x == b      = encode xs [(a+1, b)]
--      | otherwise   = [(a, b)] ++ encode xs []

