import Data.Semigroup

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (||) False . map p

data Colour
  = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  | Clear
  deriving (Show, Eq)

instance Semigroup Colour where
  Red <> Blue = Purple
  Blue <> Red = Purple
  Yellow <> Blue = Green
  Blue <> Yellow = Green
  Yellow <> Red = Orange
  Red <> Yellow = Orange
  a <> Clear = a
  Clear <> b = b
  a <> b
    | a == b = a
    | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
    | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
    | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
    | otherwise = Brown

-- Q17.1

instance Monoid Colour where
  mempty = Clear
  mappend = (<>)

-- type Events = [String]

-- type Probs = [Double]

data PTable = PTable Events Probs

showPair :: String -> Double -> String
showPair e p = mconcat [e, " | ", show p, "\n"]

instance Show PTable where
  show (PTable (Events es) (Probs ps)) = mconcat pairs
    where
      pairs = zipWith showPair es ps

createPTable :: Events -> Probs -> PTable
createPTable es (Probs ps) = PTable es (Probs normPs)
  where
    totalPs = sum ps
    normPs = map (/ totalPs) ps

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine f xs ys = zipWith f newXs cycledYs
  where
    n = length ys
    repeatedXs = map (replicate n) xs
    newXs = mconcat repeatedXs
    cycledYs = cycle ys

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events (cartCombine f e1 e2)
  where
    f x y = mconcat [x, " - ", y]

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs (cartCombine (*) p1 p2)

instance Semigroup PTable where
  PTable e1 p1 <> PTable e2 p2 = createPTable e3 p3
    where
      e3 = combineEvents e1 e2
      p3 = combineProbs p1 p2

instance Monoid PTable where
  mempty = PTable mempty mempty
  mappend = (<>)

coin :: PTable
coin = createPTable (Events ["heads", "tails"]) (Probs [0.5, 0.5])

spinner :: PTable
spinner =
  createPTable
    (Events ["red", "blue", "green"])
    ( Probs [0.1, 0.2, 0.7]
    )

data Events = Events [String]

data Probs = Probs [Double]

instance Semigroup Events where
  (<>) = combineEvents

instance Semigroup Probs where
  (<>) = combineProbs

instance Monoid Events where
  mempty = Events []
  mappend = (<>)

instance Monoid Probs where
  mempty = Probs []
  mappend = (<>)
