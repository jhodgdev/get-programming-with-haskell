data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where
    halfAlphabet = div alphabetSize 2
    offset = fromEnum c + halfAlphabet
    rotation = mod offset alphabetSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)
