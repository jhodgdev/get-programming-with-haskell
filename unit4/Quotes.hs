import qualified Data.Map as Map

quotes :: Map.Map Int String
quotes = Map.fromList qs
  where
    qs :: [(Int, String)]
    qs =
      [ (1, "Gonna make him an offer, he can't refuse."),
        (2, "Gentlemen, you can't fight in here! This is the war room!"),
        (3, "I gots the meat, Jack."),
        (4, "You ain't seen Bad Boys 2?"),
        (5, "We're on a mission from God.")
      ]

main :: IO ()
main = do
  inputs <- getContents
  let ls = lines inputs :: [String]
  mapM_ putStrLn $ quoteParse ls

quoteParse :: [String] -> [String]
quoteParse [] = []
quoteParse ("n" : rest) = []
quoteParse (x : rest) = message : quoteParse rest
  where
    n = read x :: Int
    message = case Map.lookup n quotes of
      Just q -> q ++ " Would you like another? "
      Nothing -> ""
