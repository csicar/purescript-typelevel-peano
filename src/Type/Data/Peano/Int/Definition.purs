module Type.Data.Peano.Int.Definition where

import Prelude
import Type.Data.Peano.Nat (class IsNat, Succ, Z, reflectNat, Nat)
import Type.Data.Peano.Nat.Definition (class IsZeroNat)
import Prim as Prim
import Unsafe.Coerce (unsafeCoerce)

-- | Represents a whole Number ℤ
-- |
-- | Note: Pos Z and Neg Z both represent 0
data Int

-- | Represents a posivite number
-- |
-- | ```purescript
-- | Pos (Succ Z) ^= + 1
-- | ```
-- |
foreign import data Pos :: Nat -> Int

-- | Represents a negative number
-- |
-- | ```purescript
-- | Neg (Succ Z) ^= - 1
-- | ```
-- |
foreign import data Neg :: Nat -> Int

data IProxy (i :: Int) = IProxy

class IsInt (i :: Int) where
  -- | reflect a type-level Int to a value-level Int
  -- |
  -- | ```purescript
  -- | reflectInt (Proxy  :: _ N10) = -10
  -- | reflectInt (IProxy :: _ N10) = -10
  -- | ```
  -- |
  reflectInt :: forall proxy. proxy i -> Prim.Int

instance isIntPos ∷ IsNat n => IsInt (Pos n) where
  reflectInt _ = reflectNat (unsafeCoerce unit :: _ n)

instance isIntNeg :: IsNat n => IsInt (Neg n) where
  reflectInt _ = -reflectNat (unsafeCoerce unit :: _ n)

instance showIProxy :: IsInt i => Show (IProxy i) where
  show = show <<< reflectInt

showInt :: forall proxy i. IsInt i => proxy i -> Prim.String
showInt = show <<< reflectInt

-- Addition
class SumInt (a :: Int) (b :: Int) (c :: Int) | a b -> c

-- (+ S a) + (+ b) = (+ S (a + b))
instance addPos :: (SumInt (Pos a) (Pos b) (Pos c')) => SumInt (Pos (Succ a)) (Pos b) (Pos (Succ c'))

-- (Pos (Succ a)) + (Neg (Succ b)) = a+b
instance addPosNegSucc ∷ (SumInt (Pos a) (Neg b) c) => SumInt (Pos (Succ a)) (Neg (Succ b)) c

instance addNegPosSucc :: (SumInt (Neg a) (Pos b) c) => SumInt (Neg (Succ a)) (Pos (Succ b)) c

instance addNegSucc ∷ (SumInt (Neg a) (Neg b) (Neg c')) => SumInt (Neg (Succ a)) (Neg b) (Neg (Succ c'))

instance addPosZ :: SumInt (Pos Z) (Pos b) (Pos b)

instance addNegZ :: SumInt (Neg Z) (Neg b) (Neg b)

instance addNegPosZ :: SumInt (Neg (Succ a)) (Pos Z) (Neg (Succ a))

instance addPosNegZ :: SumInt (Pos Z) (Neg (Succ b)) (Neg (Succ b))

instance minusZ :: SumInt (Pos a) (Neg Z) (Pos a)

instance minusZ' :: SumInt (Neg Z) (Pos a) (Pos a)

plus :: ∀ proxy a b c. SumInt a b c => proxy a -> proxy b -> proxy c
plus _ _ = unsafeCoerce unit

-- Inverse
-- | Invert the sign of a value (except for 0, which always stays positive)
-- |
-- | ```purescript
-- | Inverse (Pos (Succ Z)) ~> Neg (Succ Z)
-- | Inverse (Pos Z) ~> Pos Z
-- | ```
-- |
class Inverse (a :: Int) (b :: Int) | a -> b, b -> a

instance inversePosZ :: Inverse (Pos Z) (Pos Z)
else instance inversePosSucc :: Inverse (Pos a) (Neg a)
else instance inverseNegZ ∷ Inverse (Neg Z) (Pos Z)
else instance inverseNegSucc :: Inverse (Neg a) (Pos a)

-- Product
class ProductInt (a :: Int) (b :: Int) (c :: Int) | a b -> c

instance productPosNeg :: ProductInt (Pos a) (Pos b) (Pos c) => ProductInt (Neg a) (Pos b) (Neg c)

instance productNegNeg :: ProductInt (Pos a) (Pos b) (Pos c) => ProductInt (Neg a) (Neg b) (Pos c)

instance productZ :: ProductInt (Pos Z) a (Pos Z)
else instance product1 :: ProductInt (Pos (Succ Z)) a a
else instance productNegPos :: ProductInt (Pos a) (Pos b) (Pos c) => ProductInt (Pos a) (Neg b) (Neg c)
else -- (1 + a) * b = b + (a * b)
instance productSucc :: (ProductInt (Pos a) b ab, SumInt ab b result) => ProductInt (Pos (Succ a)) b result

prod :: ∀ proxy a b c. ProductInt a b c => proxy a -> proxy b -> proxy c
prod _ _ = unsafeCoerce unit

-- Is Zero
class IsZeroInt (int :: Int) (isZero :: Prim.Boolean) | int -> isZero

instance isZeroPos :: (IsZeroNat a isZero) => IsZeroInt (Pos a) isZero

instance isZeroNeg :: (IsZeroNat a isZero) => IsZeroInt (Neg a) isZero
