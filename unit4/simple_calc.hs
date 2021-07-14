import Data.List.Split

data Operation = Sum | Mul deriving (Show)

main :: IO ()
main = do
  cs <- getContents
  let ls = lines cs
  let result = map parseLine ls
  mapM_ print result

parseLine :: String -> Int
parseLine s = performOp op args
  where
    op = getOp s
    args = getArgs op s

sampleSum = "40 + 2"

sampleMul = "13 * 4"

getOp :: String -> Operation
getOp s
  | containsPlus = Sum
  | containsMul = Mul
  | otherwise = error "Neither + nor * found."
  where
    containsPlus = '+' `elem` s
    containsMul = '*' `elem` s

getArgs :: Operation -> String -> [String]
getArgs Sum = splitOn "+"
getArgs Mul = splitOn "*"

performOp :: Operation -> [String] -> Int
performOp op xs = x `f` y
  where
    f = case op of
      Sum -> (+)
      Mul -> (*)
    x = read $ xs !! 0
    y = read $ xs !! 1
