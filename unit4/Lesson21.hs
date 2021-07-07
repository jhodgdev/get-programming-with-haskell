-- Lesson 21. Hello World! - introducting IO types --
import qualified Data.Map as Map

helloPerson :: String -> String 
helloPerson name = mconcat ["Hello, ", name, "!"]

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- getLine 
  let statement = helloPerson name
  putStrLn statement

-- Q21.1

inputMap :: Map.Map Int String
inputMap = Map.fromList [(0, "Jack")]


maybeMain :: Maybe String
maybeMain = do
  name <- Map.lookup 0 inputMap
  let statement = helloPerson name
  return statement
