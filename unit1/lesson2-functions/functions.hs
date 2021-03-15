simple :: p -> p
simple x = x

-- calcChange owed given =
--   if given - owed > 0
--     then given - owed
--     else 0

calcChange :: (Num p, Ord p) => p -> p -> p
calcChange owed given =
  if change > 0
    then change
    else 0
  where
    change = given - owed

doublePlusTwo :: (Num p) => p -> p
doublePlusTwo x = doubleX + 2
  where
    doubleX = 2 * x

inc :: (Num p) => p -> p
inc = (+) 1

double :: (Num p) => p -> p
double = (*) 2

square :: (Num p) => p -> p
square x = x * x

foo :: (Integral a) => a -> a
foo n =
  if even n
    then n - 2
    else 3 * n + 1
