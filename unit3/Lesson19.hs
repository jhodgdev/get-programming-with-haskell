-- Lesson 19. The Maybe type: dealing with missing values --

import Data.List
import Data.Maybe
import qualified Data.Map as Map

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalogue :: Map.Map Int Organ
organCatalogue = Map.fromList organPairs

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalogue = map getContents ids
  where
    getContents id = Map.lookup id catalogue

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalogue

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length $ filter (== Just organ) available

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just o) = show o
showOrgan Nothing = ""

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList

numOrZero :: Maybe Int -> Int
numOrZero Nothing = 0
numOrZero (Just n) = n

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ) = show organ ++ " in a bag"

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

data Location = Lab | Kitchen | Bathroom deriving (Show)

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process = placeInLocation . organToContainer

report :: (Location, Container) -> String
report (location, container) =
  show container
    ++ " in the "
    ++ show location

processAndReport :: Maybe Organ -> String
processAndReport (Just organ) = report $ process organ
processAndReport Nothing = "ERROR! ID not found."

processRequest :: Int -> Map.Map Int Organ -> String 
processRequest id catalogue = processAndReport organ
  where
    organ :: Maybe Organ
    organ = Map.lookup id catalogue

-- Q19.1
emptyDrawers :: [Int] -> Map.Map Int Organ -> Int 
emptyDrawers ids catalogue = length $ filter isNothing content
  where
    content :: [Maybe Organ]
    content = getDrawerContents ids catalogue

-- Q19.2

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing 
maybeMap f (Just x) = Just (f x)
