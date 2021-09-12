import qualified Data.Map as Map
import System.Environment

successfulRequest :: Maybe Int
successfulRequest = Just 6

failedRequest :: Maybe Int
failedRequest = Nothing

-- QC 27.1
reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing = Nothing
reverseMaybe (Just xs) = Just (reverse xs)

-- instance Functor Maybe where
--   fmap f (Just x) = Just (f x)
--   fmap f Nothing = Nothing

-- QC 27.2
revString :: Maybe String
revString = fmap reverse (Just "Hello, World!")

revString' :: Maybe String
revString' = reverse <$> Just "Hello, World!"

data RobotPart = RobotPart
  { name :: String,
    description :: String,
    cost :: Double,
    count :: Int
  }
  deriving (Show)

leftArm :: RobotPart
leftArm =
  RobotPart
    { name = "left arm",
      description = "left arm for face punching!",
      cost = 1000.00,
      count = 3
    }

rightArm :: RobotPart
rightArm =
  RobotPart
    { name = "right arm",
      description = "right arm for kind hand gestures",
      cost = 1025.00,
      count = 5
    }

robotHead :: RobotPart
robotHead =
  RobotPart
    { name = "robot head",
      description = "this head looks mad",
      cost = 5092.25,
      count = 2
    }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part =
  mconcat
    [ "<h2>",
      partName,
      "</h2>",
      "<p><h3>desc</h3>",
      partDesc,
      "</p><p><h3>cost</h3>",
      partCost,
      "</p><p><h3>count</h3>",
      partCount,
      "</p>"
    ]
  where
    partName = name part
    partDesc = description part
    partCost = show (cost part)
    partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1 ..]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

insertSnippet :: Maybe Html -> IO ()
insertSnippet = undefined

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = map snd $ Map.toList partsDB

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

-- QC 27.3
allParts' :: [RobotPart]
allParts' = snd <$> Map.toList partsDB

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

-- Q27.1
newtype Box a = Box a deriving (Show)

instance Functor Box where
  fmap f (Box x) = Box (f x)

morePresents :: Int -> Box a -> Box [a]
morePresents n box = replicate n <$> box

-- Q27.2
myBox :: Box Int
myBox = Box 1

unwrap :: Box a -> a
unwrap (Box x) = x

-- Q27.3
-- main :: IO ()
-- main = do
--   args <- getArgs
--   let id = read $ head args
--   let part = Map.lookup id partsDB
--   print part

printCost :: Maybe Double -> IO ()
printCost Nothing = putStrLn "item not found"
printCost (Just x) = print x

main :: IO ()
main = do
  putStrLn "enter part number"
  partNo <- getLine 
  let part = Map.lookup (read partNo) partsDB
  printCost (cost <$> part)
