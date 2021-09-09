import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import System.Environment (getArgs)

-- Q25.1 --

main :: IO ()
main = do
  -- Read the file passed in through the CL.
  args <- getArgs 
  let fileName = head args 
  input <- B.readFile fileName 

  -- Number of bytes is the length of the bytestring.
  putStrLn "Bytes: "
  print (B.length input)

  -- Need to decode text appropriately.
  let decoded = E.decodeUtf8 input

  -- Number of characters is the length of the decoded text.
  putStrLn "Characters: "
  print (T.length decoded)
