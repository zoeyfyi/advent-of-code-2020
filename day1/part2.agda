module day1.part2 where

open import Prelude
open import day1.Base

part2 : List String → String
part2 ls = fromMaybe "bad input" (maybeMap fmt pair)
  where
    nums = catMaybes (map stringToℕ ls)
    pair = find-triple (λ { (x , y , z) → (x + y + z) ≟ 2020 }) nums
    fmt : (ℕ × ℕ × ℕ) → String
    fmt (x , y , z) = show (x * y * z)
    
main = getInput >>= putStrLn ∘ toCostring ∘ part2
