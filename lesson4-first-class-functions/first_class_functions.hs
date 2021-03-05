import Data.List

ifEvenInc n =
  if even n
    then n + 1
    else n

ifEven foo x =
  if even x
    then foo x
    else x

cube = \x -> x ^ 3

ifEvenCube = ifEven cube

names =
  [ ("Ian", "Curtis"),
    ("Bernard", "Sumner"),
    ("Peter", "Hook"),
    ("Stephen", "Morris")
  ]

compareLastNames :: (String, String) -> (String, String) -> Ordering
compareLastNames name1 name2
  | lastName1 > lastName2 = GT
  | lastName1 < lastName2 = LT
  | firstName1 > firstName2 = GT
  | firstName1 < firstName2 = LT
  | otherwise = EQ
  where
    firstName1 = fst name1
    lastName1 = snd name1
    firstName2 = fst name2
    lastName2 = snd name2

nyOffice = undefined

sfOffice = undefined

renoOffice = undefined

getFunctionLocation location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  _ -> \name -> fst name ++ " " ++ snd name