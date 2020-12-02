module Base where

both :: (a -> b) -> (a, a) -> (b, b)
both f (l, r) = (f l, f r)

parseLine :: String -> (Int, Int, Char, String)
parseLine str = (mini, maxi, letter, ws !! 2)
  where
    ws = words str
    (mini, maxi) = both (abs . read) $ break (== '-') (head ws)
    letter = head (ws !! 1)
