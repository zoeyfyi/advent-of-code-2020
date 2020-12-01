module Prelude where

--
-- HERE BE DRAGONS
--
-- this works, and thats good enough for me ¯\_(ツ)_/¯
--

open import Function public

open import Data.Nat public
open import Data.Nat.Show public
open import Data.List hiding (fromMaybe) public
open import Data.Product using (_×_; _,_; ∃; ∃-syntax) public
open import Data.Maybe
  using (Maybe; just; nothing; _<∣>_; fromMaybe; Is-just)
  renaming (map to maybeMap) public
open import Data.Bool using (Bool; true; false; if_then_else_) public
open import Data.Char using (Char) public
open import Data.String using (String; toList; fromList) public
--open import Data.Unit.Polymorphic using () public

open import Codata.Musical.Costring using (toCostring) public

open import Relation.Nullary public
open import Relation.Unary using (Pred; Decidable) public
open import Relation.Binary.PropositionalEquality using (_≡_; _≢_; refl) public

open import IO.Primitive public


charToℕ : Char → Maybe ℕ
charToℕ '0' = just 0
charToℕ '1' = just 1
charToℕ '2' = just 2
charToℕ '3' = just 3
charToℕ '4' = just 4
charToℕ '5' = just 5
charToℕ '6' = just 6
charToℕ '7' = just 7
charToℕ '8' = just 8
charToℕ '9' = just 9
charToℕ _   = nothing

stringToℕ : String → Maybe ℕ
stringToℕ s = stringToℕ' (toList s) 0 
  where
    stringToℕ' : List Char → (acc : ℕ) → Maybe ℕ 
    stringToℕ' []       acc = just acc
    stringToℕ' (x ∷ xs) acc = charToℕ x Data.Maybe.>>= λ n → stringToℕ' xs ( 10 * acc + n )

catMaybes : ∀ {A : Set} → List (Maybe A) → List A
catMaybes [] = []
catMaybes (nothing ∷ xs) = catMaybes xs
catMaybes (just x ∷ xs) = x ∷ (catMaybes xs)

postulate
  getLine : IO (List Char)
  isEOF : IO Bool

{-# FOREIGN GHC import qualified System.IO    #-}
{-# COMPILE GHC getLine = getLine #-}
{-# COMPILE GHC isEOF = System.IO.isEOF #-}

getLine' : IO String
getLine' = getLine >>= λ x → return (fromList x)

{-# TERMINATING #-}
getInput : IO (List String)
getInput = do
  end ← isEOF
  if end then return [] else do
    line ← getLine'
    lines ← getInput
    return (line ∷ lines)
