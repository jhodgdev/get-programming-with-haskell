type FirstName = String

type LastName = String

type Age = Int

type Height = Int

type PatientName = (String, String)

firstName :: PatientName -> FirstName
firstName patient = fst patient

lastName :: PatientName -> LastName
lastName patient = snd patient

patientInfo :: PatientName -> Age -> Height -> String
patientInfo (fName, lName) age height = formattedName ++ " " ++ ageHeight
  where
    formattedName = lName ++ ", " ++ fName
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg

data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName

-- data Patient = Patient Name Sex Int Int Int BloodType

-- johnDoe :: Patient
-- johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

-- janeSmith :: Patient
-- janeSmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 42 68 130 (BloodType O Neg)

data Patient = Patient
  { name :: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
  }

jackieSmith :: Patient
jackieSmith =
  Patient
    { name = Name "Jackie" "Smith",
      age = 43,
      sex = Female,
      height = 62,
      weight = 115,
      bloodType = BloodType O Neg
    }

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

jackieSmithUpdated :: Patient
jackieSmithUpdated = jackieSmith {age = 44}

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

-- Q12.1
canDonateToPatient :: Patient -> Patient -> Bool
canDonateToPatient p1 p2 = foo (bloodType p1) (bloodType p2)
  where
    foo :: BloodType -> BloodType -> Bool
    foo (BloodType O _) _ = True
    foo _ (BloodType AB _) = True
    foo (BloodType A _) (BloodType A _) = True
    foo (BloodType B _) (BloodType B _) = True
    foo _ _ = False

