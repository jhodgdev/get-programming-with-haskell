minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree x y z = min x $ min y z

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

main :: IO ()
main = do
  putStrLn "Enter three numbers"
  minInt <- minOfInts
  putStrLn $ show minInt ++ " is the smallest."

-- QC 28.4
maybeResult :: Maybe Int
maybeResult = minOfThree <$> Just 10 <*> Just 3 <*> Just 6
