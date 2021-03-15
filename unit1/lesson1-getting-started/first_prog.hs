messyMain :: IO ()
messyMain = do
  print "Who is the email for?"
  recipient <- getLine
  print "What is the Title?"
  title <- getLine
  print "Who is the Author?"
  author <- getLine
  print
    ( "Dear " ++ recipient ++ ",\n"
        ++ "Thanks for buying "
        ++ title
        ++ "\nthanks,\n"
        ++ author
    )

main :: IO()
main = do
  print "Who is this email for?"
  recipient <- getLine 
  print "What is the title?"
  title <- getLine 
  print "Who is the author?"
  author <- getLine 
  print (createEmail recipient title author)

createEmail :: String -> String -> String -> String
createEmail recipient bookTitle author =
  toPart recipient ++ bodyPart bookTitle ++ fromPart author

toPart :: String -> String
toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart :: String -> String
bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"

fromPart :: String -> String
fromPart author = "Thanks,\n" ++ author