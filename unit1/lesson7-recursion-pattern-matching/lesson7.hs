take' :: Int -> [a] -> [a]
take' n (x : xs)
  | n <= 0 = []
  | otherwise = x : take' (n - 1) xs

gcd' :: Integral a => a -> a -> a
gcd' x y
  | z == 0 = y
  | otherwise = gcd' y z
  where
    z = mod x y

myTail :: [a] -> [a]
myTail (_ : xs) = xs
myTail [] = error "No tail of an empty list."
