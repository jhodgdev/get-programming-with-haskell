import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import System.IO

main :: IO ()
main = do
  input <- getArgs
  let filePath = head input
  contents <- TIO.readFile filePath
  let capContents = T.toUpper contents
  TIO.writeFile filePath capContents
  putStrLn "done!"
