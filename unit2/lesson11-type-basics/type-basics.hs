-- myAverage aList = sum aList / length aList

x :: Int
x = 2

y :: Integer
y = 2

half :: Integer -> Double
half n = x / 2
  where
    x = fromIntegral n

halve :: Integer -> Integer
halve n = n `div` 2

printDouble :: Int -> String
printDouble n = show (n * 2)

anotherNumber :: Int
anotherNumber = read "6"

-- Q11.1
filter' :: (a -> Bool) -> [a] -> [a]
filter' = undefined

-- Q11.2
head :: [a] -> a
head (x : xs) = x

tail :: [a] -> [a]
tail (x : xs) = xs

-- No implementation of head can return the empty list as head does not necessarily return a list.

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x : xs) = myFoldl f newInit xs
  where
    newInit = f init x
