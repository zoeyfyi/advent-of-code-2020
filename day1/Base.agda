module day1.Base where

open import Prelude

open import Data.List.Relation.Unary.Any hiding (lookup)

find : ∀ {A : Set} {p} {P : Pred A p} → Decidable P → List A → Maybe A
find P [] = nothing
find P (x ∷ xs) with does (P x)
... | false = find P xs
... | true = just x

find-pair : ∀ {A : Set} {p} {P : Pred (A × A) p} → Decidable P → List A → Maybe (A × A)
find-pair P [] = nothing
find-pair P (x ∷ xs) = found <∣> find-pair P xs
  where found = maybeMap (_,_ x) (find (λ y → P (x , y)) xs)

find-triple : ∀ {A : Set} {p} {P : Pred (A × A × A) p} → Decidable P → List A → Maybe (A × A × A)
find-triple P [] = nothing
find-triple P (x ∷ xs) = found <∣> (find-triple P xs)
  where found = maybeMap (_,_ x) (find-pair (λ yz → P (x , yz)) xs)

find⇒P : ∀ {A : Set} {p} {P : Pred A p} (P? : Decidable P) (xs : List A) (y : A) →
  find P? xs ≡ just y → P y
find⇒P P (x ∷ xs) y p  with P x
... | false because proof₁ = find⇒P P xs y p
find⇒P P (x ∷ xs) x refl | yes Px = Px

find⇒AnyP : ∀ {A : Set} {p} {P : Pred A p} (P? : Decidable P) (xs : List A) (y : A) →
  find P? xs ≡ just y →
  Any P xs
find⇒AnyP _ [] y ()
find⇒AnyP P (x ∷ xs) y isJust with P x
... | false because p = there (find⇒AnyP P xs y isJust)
... | yes p = here p
