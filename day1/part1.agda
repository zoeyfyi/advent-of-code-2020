module day1.part1 where

open import Prelude
open import day1.Base

part1 : List String → String
part1 ls = fromMaybe "bad input" (maybeMap fmt pair)
  where
    nums = catMaybes (map stringToℕ ls)
    pair = find-pair (λ { (x , y) → (x + y) ≟ 2020 }) nums
    fmt : (ℕ × ℕ) → String
    fmt (x , y) = show (x * y)
    
main = getInput >>= putStrLn ∘ toCostring ∘ part1
