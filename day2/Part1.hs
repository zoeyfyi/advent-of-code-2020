module Part1 where

import Base ( parseLine )

isValid :: (Int, Int, Char, String) -> Bool
isValid (mini, maxi, letter, password) = count >= mini && count <= maxi
  where
    count = length $ filter (== letter) password

main :: IO ()
main = do
  input <- getContents
  print $ length $ filter isValid $ map parseLine $ lines input