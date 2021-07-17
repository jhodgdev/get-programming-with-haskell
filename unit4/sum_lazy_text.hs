import Data.Text.Lazy as T
import Data.Text.Lazy.IO as TIO

toInts :: T.Text -> [Int]
toInts = Prelude.map (read . T.unpack) . T.lines

main :: IO ()
main = do
  userInput <- TIO.getContents
  let numbers = toInts userInput
  TIO.putStrLn $ T.pack $ show $ sum numbers
