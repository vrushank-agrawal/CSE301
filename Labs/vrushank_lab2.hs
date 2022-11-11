import Data.Maybe

--- Proving exercises (from Lecture 1 notes)

-- Exercise 3.1
{-
    In this exercise, the function ++ is of the following type:
    (++) :: [a] -> [a] -> [a]
    Here, the function takes two input lists of which, without
    loss of generality we say that the second parameter of the
    input remains constant while recursion happens in the first
    element. Essentially, the base case be written as:

            [] ++ xs = xs           -- (1)

    For the second condition, we can use structural induction as:

            xs ++ [] = (x:xs) ++ []
                     = x:(xs' ++ [])
                     = x:xs'
                     = xs           -- (2)

    Hence, we have proved the second point.

    Now, let us take three lists xs, ys, and zs. Essentially,

        xs ++ (ys ++ zs) = xs ++ ((y:ys)++zs)
                         = xs ++ (y:(ys++zs))
                         = xs ++ (y:yzs')
                         = xs ++ (yzs)
                         = (x:xs) ++ yzs
                         = x:(xs ++ yzs)
                         = x:xyzs'
                         = xyzs
                         = xys ++ zs
                         = (xys) ++ zs
                         = (x:xys') ++ zs
                         = (x:(xs++ys)) ++ zs
                         = ((x:xs)++ys) ++ zs
                         = (xs ++ ys) ++ zs         -- (3)

       where xs++ys = xys, ys++zs = yzs, xs++ys++zs = xyzs

    We have hence, proved the third equation.

-}

-- Exercise 3.2 (optional)

-- Exercise 3.3 (optional)

-- Exercise 3.4 (optional)

--- Programming exercises

-- Exercise 0a
doubleList :: [a] -> [a]
doubleList [] = []
doubleList (x:xs) = [x, x] ++ doubleList xs

-- Exercise 0b
firstDoubled :: Eq a => [a] -> Maybe a
firstDoubled [] = Nothing
firstDoubled [x] = Nothing
firstDoubled (x:xs@(y:ys))
      | x == y      = Just x
      | otherwise   = firstDoubled xs

------------------------------------------------------------

data Allergen = Nuts | Gluten | Soy | Dairy      deriving (Show, Eq)

type Recipe   = [Allergen]

type Name     = String
type Price    = Int
data Cupcake  = CC Name Recipe Price             deriving (Show,Eq)

r1, r2, r3, r4, r5 :: Recipe
r1 = [Gluten]
r2 = []
r3 = [Nuts]
r4 = [Dairy,Gluten]
r5 = [Soy]

onsale :: [Cupcake]
onsale = [CC "Chocolate Surprise" r1 200,
          CC "Lemon Mayhem" r2 150,
          CC "Peanut Butter Bliss" r3 150,
          CC "Yogurt Truly" r4 250,
          CC "Caramel Karma" r5 200]

getPrice :: Cupcake -> Price
getPrice (CC _ _ a) = a

getName :: Cupcake -> Name
getName (CC a _ _) =  a

-- Exercise 1a
priceRange :: Price -> Price -> [Cupcake] -> [Name]
priceRange x y [] = []
priceRange x y (z:zs)
      | (p <= y) && (p >= x)  = [n] ++ priceRange x y zs
      | otherwise              = priceRange x y zs
      where p = getPrice z
            n = getName z

getAllergen :: Cupcake -> Recipe
getAllergen (CC _ a _ ) = a

--Return False if allergen present
isAllergenPresent :: [Allergen] -> Recipe -> Bool
isAllergenPresent [] _ = True
isAllergenPresent _ [] = True
isAllergenPresent (x:xs) y
      | x `elem` y    = False
      | otherwise   = isAllergenPresent xs y

-- Exercise 1b
allergyFree :: [Allergen] -> [Cupcake] -> [Name]
allergyFree _ [] = []
allergyFree x (z:zs)
      | check     = [n] ++ allergyFree x zs
      | otherwise = allergyFree x zs
      where check = isAllergenPresent x (getAllergen z)
            n = getName z

------------------------------------------------------------

type Tin = [Recipe]
data Spec = And Spec Spec | Or Spec Spec | Not Spec | HasCup Int Allergen  deriving (Show,Eq)

sampletin :: Tin
sampletin = [r3,r4,r2,r5]

-- Exercise 2a
checkSpec :: Spec -> Tin -> Bool
checkSpec _ []            = True
checkSpec (And x y) t     = (checkSpec x t) && (checkSpec y t)
checkSpec (Or x y) t      = (checkSpec x t) || (checkSpec y t)
checkSpec (Not x) t       = not (checkSpec x t)
checkSpec (HasCup i a) t  = not (isAllergenPresent [a] (t !! i)) -- We have not because isAllergenPresent returns False if allergen is present

-- Exercise 2b (optional)
checkSpec' :: Spec -> Tin -> Maybe Bool
checkSpec' _ []            = Just True
checkSpec' (And x y) t
      | (isNothing checkx) || (isNothing checky) = Nothing
      | otherwise       = Just (fromJust checkx && fromJust checky)
      where checkx = checkSpec' x t
            checky = checkSpec' y t
checkSpec' (Or x y) t
      | (isNothing checkx) || (isNothing checky) = Nothing
      | otherwise       = Just (fromJust checkx || fromJust checky)
      where checkx = checkSpec' x t
            checky = checkSpec' y t
checkSpec' (Not x) t
      | isNothing check = Nothing
      | otherwise       = Just (not (fromJust check))
      where check = checkSpec' x t
checkSpec' (HasCup i a) t
      | i < length t    = Just (not (isAllergenPresent [a] (t !! i)))
      | otherwise       = Nothing

------------------------------------------------------------

data Tree a b = Leaf a | Node b [Tree a b]  deriving (Show,Eq)

texample :: Tree Char Integer
texample = Node 1 [Node 2 [Leaf 'a', Leaf 'b'], Node 3 [Leaf 'c', Leaf 'd', Leaf 'e'], Node 4 []]

bst :: Tree () Char
bst = Node 'c' [Node 'a' [Leaf (), Node 'b' [Leaf (), Leaf ()]], Node 'd' [Leaf (), Leaf ()]]

-- Exercise 3a
canopy :: Tree a b -> [a]
canopy (Node a [])     = []
canopy (Leaf a)        = [a]
canopy (Node a (t:ts)) = (canopy t) ++ (canopy (Node a ts))

-- Exercise 3b (optional)
preorder' :: [Tree a b] -> [Either a b]
preorder' []                   = []
preorder' [Node a []]          = [Right a]
preorder' ((Leaf a):xs)        = (Left a) : preorder' xs
preorder' (Node a (t:ts):xs)   = (Right a) : ((preorder' [t]) ++ (preorder' ts) ++ (preorder' xs))

preorder :: Tree a b -> [Either a b]
preorder a = preorder' [a]

------------------------------------------------------------

auxLSort :: Ord a => [a] -> [a] -> [a] -> [a]
auxLSort [] x y       = y ++ x
auxLSort (x:xs) [] y  = auxLSort xs [x] y
auxLSort (x:xs) (y:ys) z
      | x <= y    = auxLSort xs (x:(y:ys)) z
      | otherwise = auxLSort (x:xs) ys (z++[y])

-- Exercise 4
linearSort :: Ord a => [a] -> [a]
linearSort []  = []
linearSort [a] = [a]
linearSort a   = auxLSort a [] []

------------------------------------------------------------

-- Exercise 5a (optional)
counterexample :: [Int]
counterexample = [2,3,1]

data Bin = L | B Bin Bin  deriving (Show,Eq)

--mergeBin :: B -> [Int] -> [Int] -> [Int]
--mergeBin B [] [] = [B]
--mergeBin B xs [] = mergeBin B [] xs
--mergeBin B [] (x:xs)
--      | B <= x   = B:x:xs
--      | otherwise=

-- Exercise 5b (optional)
fromBin :: Bin -> [Int]
fromBin [] = undefined
--fromBin L  = [L]
--fromBin (B Bin Bin) = mergeBin (B fromBin(Bin) fromBin(Bin))


toBin :: [Int] -> Maybe Bin
toBin = undefined
