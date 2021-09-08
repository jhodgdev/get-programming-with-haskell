import Control.Monad (foldM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Environment
import System.Random

-- QC 25.3
glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions =
  [ randomReplaceByte,
    randomSortSection,
    randomReplaceByte,
    randomSortSection,
    randomReplaceByte
  ]

main :: IO ()
main = do
  args <- getArgs -- Using getArgs to access the filename.
  let fileName = head args -- First and only argument is filename.
  imageFile <- BC.readFile fileName -- Read the file.
  glitched <- foldM (\bytes func -> func bytes) imageFile glitchActions
  let glitchedFileName = mconcat ["glitched_", fileName] -- Construct new file name.
  BC.writeFile glitchedFileName glitched -- Write the glitched file.
  print "all done"

-- | Converts an integer into a Char.
intToChar :: Int -> Char
intToChar n = toEnum safeN
  where
    safeN = mod n 255

-- | Converts an integer into a valid ByteString.
intToBC :: Int -> BC.ByteString
intToBC n = BC.pack [intToChar n]

-- | Removes a byte and replaces it with a new one.
replaceByte ::
  -- | The location of the byte to replace.
  Int ->
  -- | The new value to insert into the location.
  Int ->
  -- | The original string.
  BC.ByteString ->
  -- | The new string.
  BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt loc bytes
    after = BC.drop 1 rest
    newChar = intToBC charVal

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)

randomChar :: IO Char
randomChar = do
  int <- randomRIO (0, 255)
  return (toEnum int)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25 -- arbitrary.
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)
