import Control.Monad
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let linesToRead =
        if not $ null args
          then read $ head args
          else 0 :: Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print $ sum ints

-- QC 22.1
-- main :: IO ()
-- main = do
--   args <- mapM (\_ -> getLine) [1 .. 3]
--   mapM_ putStrLn args

-- QC 22.2
myReplicateM n f = mapM (\_ -> f) [1 .. n]
