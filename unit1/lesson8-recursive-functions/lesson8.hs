drop' :: Integral a => a -> [b] -> [b]
drop' n (x : xs) = case n of
  0 -> x : xs
  n -> drop' (n - 1) xs

myLength :: Integral b => [a] -> b
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

myTake _ [] = []
myTake 0 _ = []
myTake n (x : xs) = x : rest
  where
    rest = myTake (n - 1) xs

finiteCycle :: [a] -> [a]
finiteCycle xs = xs ++ finiteCycle xs

ackermann :: Integral a => a -> a -> a
ackermann m n
  | m == 0 = n + 1
  | n == 0 = ackermann (m - 1) 1
  | otherwise = ackermann (m - 1) (ackermann m (n - 1))

collatz :: Integral a => a -> a
collatz n
  | n == 1 = 1
  | even n = 1 + collatz (div n 2)
  | otherwise = 1 + collatz (3 * n + 1)

-- Q8.1
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

-- Q8.2
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fastFib :: Integral a => a -> a
fastFib = f 0 1
  where 
    f :: Integral a => a -> a -> a -> a
    f m n c = case c of
      0 -> m
      c -> f n (n + m) (c - 1)

fastFib' :: Integral a => a -> a
fastFib' = f 1 1
  where
    f _ _ 0 = 0
    f _ _ 1 = 1
    f _ _ 2 = 1
    f x y 3 = x + y
    f x y c = f (x + y) x (c - 1)

prop_fastFib :: Int -> Bool
prop_fastFib n = fastFib n == fastFib' n