import qualified Data.Map as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB =
  Map.fromList
    [ ("Arkham", (42.6054, -70.7829)),
      ("Innsmouth", (42.8250, -70.8150)),
      ("Carcosa", (29.9714, -90.7694)),
      ("New York", (40.7776, -73.9691))
    ]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
  where
    rlat = toRadians lat
    rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
  where
    (rlat1, rlong1) = latLongToRads coords1
    (rlat2, rlong2) = latLongToRads coords2
    dlat = rlat2 - rlat1
    dlong = rlong2 - rlong1
    a = sin (dlat / 2) ^ 2 + cos rlat1 * cos rlat2 * sin (dlong / 2) ^ 2
    c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    earthRadius = 3961.0

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

-- QC 28.1
addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just m) (Just n) = Just (m + n)
addMaybe _ _ = Nothing

-- QC 28.2
newYork :: LatLong
newYork = (40.7776, -73.9691)

distanceFromNY :: LatLong -> Double
distanceFromNY = haversine newYork

-- QC 28.3
val1 = Just 10

val2 = Just 5

a = (*) <$> val1 <*> val2

b = div <$> val1 <*> val2

c = mod <$> val1 <*> val2

--

startingCity = Map.lookup "Carcosa" locationDB

destCity = Map.lookup "Innsmouth" locationDB

main :: IO ()
main = do
  putStrLn "starting city:"
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  putStrLn "destination city:"
  destInput <- getLine
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance
  print "done"
