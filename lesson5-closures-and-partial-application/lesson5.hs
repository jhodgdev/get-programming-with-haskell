ifEven :: Integral a => (a -> a) -> a -> a
ifEven f x =
  if even x
    then f x
    else x

inc :: Integer -> Integer
inc = (+) 1

genIfEven :: Integral a => (a -> a) -> (a -> a)
genIfEven f = \x -> ifEven f x

genIfXEven :: Integral a => a -> ((a -> a) -> a)
genIfXEven x = \f -> ifEven f x

getRequestURL :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
getRequestURL host apiKey resource id =
  host
    ++ "/"
    ++ resource
    ++ "/"
    ++ id
    ++ "?token="
    ++ apiKey

genHostRequestBuilder :: [Char] -> ([Char] -> [Char] -> [Char] -> [Char])
genHostRequestBuilder host =
  \apiKey resource id ->
    getRequestURL host apiKey resource id

exampleUrlBuilder :: ([Char] -> [Char] -> [Char] -> [Char])
exampleUrlBuilder = genHostRequestBuilder "http://example.com"

genApiRequestBuilder ::
  ([Char] -> [Char] -> [Char] -> [Char]) ->
  [Char] ->
  ([Char] -> [Char] -> [Char])
genApiRequestBuilder hostBuilder apiKey =
  \resource id ->
    hostBuilder apiKey resource id

myExampleUrlBuilder :: [Char] -> [Char] -> [Char]
myExampleUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"

genApiRequestBuilder' ::
  ([Char] -> [Char] -> [Char] -> [Char]) ->
  [Char] ->
  [Char] ->
  ([Char] -> [Char])
genApiRequestBuilder' hostBuilder apiKey resource =
  \id ->
    hostBuilder apiKey resource id

exampleBuilder :: [Char] -> [Char]
exampleBuilder = getRequestURL host apiKey resource
  where
    host = "https://example.com"
    apiKey = "1337hAsk3ll"
    resource = "book"

flipBinaryArgs :: (a -> b -> c) -> (b -> a -> c)
flipBinaryArgs binaryFunction = \x y -> binaryFunction y x

subtract2 :: (Integral a) => a -> a
subtract2 = flip (-) 2

-- Q5.1

ifEvenInc :: (Integral a) => a -> a
ifEvenInc = ifEven (1 +)

ifEvenDouble :: (Integral a) => a -> a
ifEvenDouble = ifEven (2 *)

ifEvenSquare :: (Integral a) => a -> a
ifEvenSquare = ifEven (^ 2)

-- Q5.2

binaryPartialApplication :: (a -> b -> c) -> a -> (b -> c)
binaryPartialApplication f x = \y -> f x y