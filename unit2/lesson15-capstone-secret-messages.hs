data FourLetterAlphabet = L1 | L2 | L3 | L4
  deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphaSize c = toEnum rotation
  where
    halfAlpha :: Int
    halfAlpha = div alphaSize 2
    offset :: Int
    offset = fromEnum c + halfAlpha
    rotation :: Int
    rotation = mod offset alphaSize

rotChar :: Char -> Char
rotChar = rotN alphaSize
  where
    alphaSize = 1 + fromEnum (maxBound :: Char)

xorBool :: Bool -> Bool -> Bool
xorBool x y = xOrY && not xAndY
  where
    xOrY = x || y
    xAndY = x && y
