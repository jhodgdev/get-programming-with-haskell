import qualified Data.ByteString as B
import System.Environment (getArgs)

-- Q25.1 --

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  text <- readFile fileName
  bytes <- B.readFile fileName
  let out = mconcat ["Chars: ", show $ length text, " Bytes: ", show $ B.length bytes]
  print out
