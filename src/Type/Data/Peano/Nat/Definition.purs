module Type.Data.Peano.Nat.Definition where

import Prelude (class Show, show, unit, ($), (+))
import Type.Prelude (EQ, GT, LT, kind Ordering, kind Boolean, True, False)
import Unsafe.Coerce (unsafeCoerce)

-- Nat
-- | Represents a non-negative whole Number ℕ₀
foreign import kind Nat

-- | Represents 0
foreign import data Z :: Nat

-- | Represents Successor of a Nat: `(Succ a) ^= 1 + a`
foreign import data Succ :: Nat -> Nat

-- | Proxy from kind Nat to kind Type
data NProxy (n :: Nat) = NProxy

class IsNat (a :: Nat) where
  -- | reflect typelevel Nat to a valuelevel Int
  -- |
  -- |
  -- | ```purescript
  -- | reflectNat (undefined :: NProxy D10) = 10
  -- | ```
  reflectNat :: NProxy a -> Int

instance isNatZ ∷ IsNat Z where
  reflectNat _ = 0

instance isNatSucc ∷ IsNat a => IsNat (Succ a) where
  reflectNat _ = 1 + (reflectNat (unsafeCoerce unit :: NProxy a))

instance showZ ∷ IsNat a => Show (NProxy a) where
  show a = show $ reflectNat (unsafeCoerce unit :: NProxy a)

-- Addition
-- | a + b = c
class SumNat (a :: Nat) (b :: Nat) (c :: Nat) | a b -> c

instance addZ ∷ SumNat a Z a
else instance addZ' ∷ SumNat Z a a
else instance addSucc :: SumNat a b c => SumNat (Succ a) b (Succ c)

plusNat :: ∀ a b c. SumNat a b c => NProxy a -> NProxy b -> NProxy c
plusNat _ _ = unsafeCoerce unit :: NProxy c

-- Product
-- | a * b = c
class ProductNat (a :: Nat) (b :: Nat) (c :: Nat) | a b -> c

--| 0 * a = 0
instance productZ' :: ProductNat Z a Z

--| 1 * a = a
instance product1' ∷ ProductNat (Succ Z) a a
else 
--| (1 + a) * b = b + a * b
instance productSucc :: (ProductNat a b ab, SumNat ab b result) => ProductNat (Succ a) b result

mulNat :: ∀ a b c. ProductNat a b c => NProxy a -> NProxy b -> NProxy c
mulNat _ _ = unsafeCoerce unit :: NProxy c

-- Exponentiation
class ExponentiationNat (a :: Nat) (b :: Nat) (c :: Nat) | a b -> c

instance exponentiationZ :: ExponentiationNat a Z (Succ Z)

instance exponentiationSucc :: (ExponentiationNat a b ab, ProductNat ab a result) => ExponentiationNat a (Succ b) result

--| ```purescript
--| > powNat d2 d3
--| 8 -- : NProxy D8
--| ```
-- | a raised to the power of b `a^b = c`
powNat :: ∀a b c. ExponentiationNat a b c => NProxy a -> NProxy b -> NProxy c
powNat _ _ = NProxy :: NProxy c

-- Compare
class CompareNat (a :: Nat) (b :: Nat) (ord :: Ordering) | a b -> ord

instance compareSame :: CompareNat a a EQ
else instance compareZ :: CompareNat Z a LT
else instance compareZ' ∷ CompareNat a Z GT
else instance compareSucc ∷ CompareNat a b ord => CompareNat (Succ a) (Succ b) ord

-- Is Zero
class IsZeroNat (a :: Nat) (isZero :: Boolean) | a -> isZero

instance isZeroZ :: IsZeroNat Z True

instance isZeroSucc :: IsZeroNat (Succ a) False
