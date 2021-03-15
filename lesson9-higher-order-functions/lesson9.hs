import Data.Char

myMap :: (t1 -> t2) -> [t1] -> [t2]
myMap _ [] = []
myMap f (x : xs) = f x : myMap f xs

add3ToAll :: Num t1 => [t1] -> [t1]
add3ToAll = myMap (+ 3)

mul3ByAll :: Num t1 => [t1] -> [t1]
mul3ByAll = myMap (* 3)

remove :: (t1 -> Bool) -> [t1] -> [t1]
remove f [] = []
remove f (x : xs) =
  if f x
    then remove f xs
    else x : remove f xs

myProduct :: Num t1 => [t1] -> t1
myProduct = foldl (*) 1

-- Q9.1
elem' :: Eq a => a -> [a] -> Bool
elem' x xs = (length $ filter (== x) xs) > 0

-- Q9.2
isPalindrome :: String -> Bool
isPalindrome s = t == reverse t
  where
    t = filter (/= ' ') $ map toLower s

-- Q9.3
harmonic n = sum (map (1 /) [1..n])