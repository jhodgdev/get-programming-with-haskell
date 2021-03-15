sumSquareOrSquareSum x y =
  if sumSquare > squareSum
    then sumSquare
    else squareSum
  where
    sumSquare = x ^ 2 + y ^ 2
    squareSum = (x + y) ^ 2

sumSquareOrSquareSum' x y =
  ( \sumSquare squareSum ->
      if sumSquare > squareSum
        then sumSquare
        else squareSum
  )
    (x ^ 2 + y ^ 2)
    ((x + y) ^ 2)

doubleDouble x = (\dubs -> dubs * 2) x * 2

sumSquareOrSquareSum'' x y =
  let sumSquare = (x ^ 2 + y ^ 2)
      squareSum = (x + y) ^ 2
   in if sumSquare > squareSum
        then sumSquare
        else squareSum

overwrite x =
  let x = 2
   in let x = 3
       in let x = 4
           in x

overwrite' x = ((\x -> 4) ((\x -> 3) ((\x -> 2) x)))

overwrite'' x = (\x -> (\x -> (\x -> x) 4) 3) 2