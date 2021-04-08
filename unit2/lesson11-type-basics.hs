-- myAverage xs = sum xs / length xs

x :: Int
x = 2

y :: Integer
y = 2

double :: Int -> Int
double n = n * 2

half :: Int -> Double
half n = fromIntegral n / 2

halve :: Integer -> Integer
halve n = n `div` 2

printDouble :: Int -> String
printDouble n = show (n * 2)

anotherNumber :: Int
anotherNumber = read "6"

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

-- Q11.1
-- map :: (a -> b) -> [a] -> [b]
-- filter :: (a -> Bool) -> [a] -> [a]

-- Q11.2
-- head :: [a] -> a
-- tail :: [a] -> [a]

-- Q11.3
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x : xs) = myFoldl f newInit xs
  where
    newInit = f init x
