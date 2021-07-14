import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let linesToRead =
        if not $ null args
          then read $ head args
          else 0 :: Int
  print linesToRead

-- QC 22.1
-- main :: IO ()
-- main = do
--   strings <- mapM (\_ -> getLine) [1..3]
--   mapM_ putStrLn strings
