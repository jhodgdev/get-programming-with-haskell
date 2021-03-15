cup flOz = \message -> message flOz

coffeeCup = cup 12

getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank = cup (flOz - ozDrank)
  where
    flOz = getOz aCup
