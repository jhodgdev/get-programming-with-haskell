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

sfOffice name =
  if lastName < "L"
    then
      nameText
        ++ " - PO Box 1234 - San Francisco, CA, 94111"
    else
      nameText
        ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where
    lastName = snd name
    nameText = fst name ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where
    nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where
    nameText = snd name

getLocationFunction location = case location of
  "ny" -> nyOffice
  "sf" -> sfOffice
  "reno" -> renoOffice
  "dc" -> dcOffice
  _ -> \name -> fst name ++ " " ++ snd name

addressLetter name location = locationFunction name
  where
    locationFunction = getLocationFunction location

-- Q4.1

compareLastNames' :: (String, String) -> (String, String) -> Ordering
compareLastNames' name1 name2 =
  if result == EQ
    then compare (fst name1) (fst name2)
    else result
  where
    result = compare (snd name1) (snd name2)

-- Q4.2

dcOffice name = nameText ++ " - PO Box 666 - Washington, D.C. 99999"
  where
    nameText = fst name ++ " " ++ snd name ++ " Esq."