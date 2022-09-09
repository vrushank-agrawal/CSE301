merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
                      then x : merge xs (y:ys)
                      else y : merge (x:xs) ys

split :: [a] -> ([a], [a])
split [] = ([], [])
split xs = (take half xs, drop half xs)
           where half = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
           where
           (ys, zs) = split (xs)


isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:xs) = if x <= head xs
                  then isSorted xs
                  else False


getLines :: IO [String]
getLines = do
           x <- getLine
           if x == ""
              then return []
              else do
                   xs <- getLines
                   return (x:xs)

main :: IO()
main = do
       putStrLn "Enter an empty line to exit input"
       line <- getLines
       mapM_ print $ (msort line)