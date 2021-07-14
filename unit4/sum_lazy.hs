import Data.List.Split

main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  print $ sum numbers

sampleData :: String
sampleData = ['6', '2', '\n', '2', '1', '\n']

toInts :: String -> [Int]
toInts = map read . lines

-- QC 22.3
-- main :: IO ()
-- main = do
--   input <- getContents
--   let revInput = reverse input
--   putStrLn revInput

-- QC 22.4
-- main :: IO ()
-- main = do
--   input <- getContents
--   let numbers = toInts input
--   print $ sum $ map (^ 2) numbers
