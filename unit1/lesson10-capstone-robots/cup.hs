cup flOz = \message -> message flOz

coffeeCup = cup 12

getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank = cup (max (flOz - ozDrank) 0)
  where
    flOz = getOz aCup

isEmpty aCup = getOz aCup == 0

robot (name, attack, hp) = \message -> message (name, attack, hp)

killerRobot = robot ("Kill3r", 25, 200)

name (n, _, _) = n

getName aRobot = aRobot name

attack (_, a, _) = a

getAttack aRobot = aRobot attack

hp (_, _, hp) = hp

getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))

setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))

setHP aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))

printRobot aRobot = aRobot (\(n, a, h) -> n ++ " attack:" ++ (show a) ++ " hp:" ++ (show h))

damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, (h - attackDamage)))

fight aRobot defender = damage defender attack
  where
    attack =
      if getHP aRobot > 0
        then getAttack aRobot
        else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)
