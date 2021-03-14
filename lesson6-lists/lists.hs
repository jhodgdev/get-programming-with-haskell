teams :: [String]
teams = ["red", "yellow", "orange", "blue", "purple"]

backwardsInfinity :: [Integer]
backwardsInfinity = reverse [1 ..]

respond :: String -> String
respond phrase =
  if '!' `elem` phrase
    then "wow!"
    else "uk... ok"

takeLast :: Int -> [a] -> [a]
takeLast n xs = reverse $ take n $ reverse xs

ones :: Num a => Int -> [a]
ones n = take n $ cycle [1]

assignToGroups :: Integral a => a -> [b] -> [(a, b)]
assignToGroups n = zip groups
  where
    groups = cycle [1 .. n]

-- Q6.1
repeat :: a -> [a]
repeat x = cycle [x]

-- Q6.2
subseq :: Int -> Int -> [a] -> [a]
subseq a b = take c . drop a 
  where c = b - a

-- Q6.3
inFirstHalf :: Eq a => a -> [a] -> Bool 
inFirstHalf x xs = x `elem` ys
  where 
    ys = take halfLen xs
    halfLen = div (length xs) 2