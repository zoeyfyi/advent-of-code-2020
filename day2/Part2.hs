module Part1 where

import Base ( parseLine )

isValid :: (Int, Int, Char, String) -> Bool
isValid (f, s, letter, password) = isValidLetter f /= isValidLetter s
    where isValidLetter pos = (pos <= length password) && (letter == password !! (pos - 1)) 

main :: IO ()
main = do
  input <- getContents
  print $ length $ filter isValid $ map parseLine $ lines input