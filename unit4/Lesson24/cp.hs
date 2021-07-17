import System.IO
import System.Environment


main :: IO ()
main = do
  input <- getArgs
  let original = head input
  let new = input !! 1
  contents <- readFile original
  writeFile new contents
  putStrLn "done!"
