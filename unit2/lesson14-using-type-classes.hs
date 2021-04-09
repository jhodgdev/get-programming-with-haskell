-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show, Eq, Ord)

-- instance Show SixSidedDie where
--   show S1 = "one"
--   show S2 = "two"
--   show S3 = "three"
--   show S4 = "four"
--   show S5 = "five"
--   show S6 = "six"

-- instance Eq SixSidedDie where
--   (==) S6 S6 = True
--   (==) S5 S5 = True
--   (==) S4 S4 = True
--   (==) S3 S3 = True
--   (==) S2 S2 = True
--   (==) S1 S1 = True
--   (==) _ _ = False

-- Q14.1
-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum, Show)

-- instance Eq SixSidedDie where
--   (==) s t = fromEnum s == fromEnum t

-- instance Ord SixSidedDie where
--   compare s t = compare (fromEnum s) (fromEnum t)

-- Q14.2
data FiveSidedDie = S1 | S2 | S3 | S4 | S5 deriving (Enum, Eq, Show)

class (Eq a, Enum a) => Die a where
  roll :: Int -> a

instance Die FiveSidedDie where
  roll n = toEnum (n `mod` 5)
