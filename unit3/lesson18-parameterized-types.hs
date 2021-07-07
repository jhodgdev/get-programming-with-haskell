import qualified Data.Map as Map

data Box a = Box a deriving (Show)

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

data Triple a = Triple a a a deriving (Show)

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.3

data List a = Empty | Cons a (List a) deriving (Show)

data Organ
  = Heart
  | Brain
  | Kidney
  | Spleen
  deriving (Eq, Ord, Show)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organCatalogue :: Map.Map Int Organ
organCatalogue = Map.fromList $ zip ids organs

tripleMap :: (a -> a) -> Triple a -> Triple a
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

boxMap :: (a -> a) -> Box a -> Box a
boxMap f (Box x) = Box (f x)

organInventory :: Map.Map Organ Int 
organInventory = Map.fromList $ zip organs organCounts
  where
    organCounts :: [Int]
    organCounts = map findCount organs
    findCount :: Organ -> Int
    findCount organ = length $ filter (== organ) organs
