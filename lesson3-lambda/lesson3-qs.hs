inc = \x -> x + 1

double = \x -> x * 2

square = \x -> x ^ 2

foo = \x ->
  if even x
    then x - 2
    else 3 * x + 1

calcChange owed given =
  ( \change ->
      if change > 0
        then change
        else 0
  )
    (given - owed)

counter x = (\x -> x + 1) ((\x -> x + 1) ((\x -> x) x))