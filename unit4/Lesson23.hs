{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup
import qualified Data.Text as T

firstWord :: String
firstWord = "optimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

aWord :: T.Text
aWord = "Cheese"

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " ", "text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " " <> "text"

myLines :: T.Text -> [T.Text]
myLines = T.splitOn "\n"

myUnlines :: [T.Text] -> T.Text 
myUnlines = T.intercalate "\n"
