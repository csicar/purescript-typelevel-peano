module Type.Data.Peano.Int where

import Prelude

import Type.Data.Boolean (class If)
import Type.Data.Peano.Nat (class IsNat, class ParseNat, NProxy, Succ, Z, D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, reflectNat, kind Nat)
import Type.Data.Symbol (class Cons, class Equals, SProxy)
import Unsafe.Coerce (unsafeCoerce)


-- | Represents a whole Number ℤ
-- |
-- | Note: Pos Z and Neg Z both represent 0
foreign import kind Int
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

data IProxy (i :: Int)


type P0  = Pos D0
type P1  = Pos D1
type P2  = Pos D2
type P3  = Pos D3
type P4  = Pos D4
type P5  = Pos D5
type P6  = Pos D6
type P7  = Pos D7
type P8  = Pos D8
type P9  = Pos D9
type P10 = Pos D10

type N0  = Neg D0
type N1  = Neg D1
type N2  = Neg D2
type N3  = Neg D3
type N4  = Neg D4
type N5  = Neg D5
type N6  = Neg D6
type N7  = Neg D7
type N8  = Neg D8
type N9  = Neg D9
type N10 = Neg D10

p0 :: IProxy P0
p0 = unsafeCoerce unit

p1 :: IProxy P1
p1 = unsafeCoerce unit

n1 :: IProxy N1
n1 = unsafeCoerce unit

class IsInt (i :: Int) where
  -- | reflect a type-level Int to a value-level Int
  -- |
  -- | ```purescript
  -- | reflectInt (undefined :: IProxy N10) = -10
  -- | ```
  -- |
  reflectInt :: IProxy i -> Int

instance isIntPos ∷ IsNat n => IsInt (Pos n) where
  reflectInt _ = reflectNat (unsafeCoerce unit :: NProxy n)

instance isIntNeg :: IsNat n => IsInt (Neg n) where
  reflectInt _ = - reflectNat (unsafeCoerce unit :: NProxy n)

instance showInt ∷ IsInt i => Show (IProxy i) where
  show _ = show $ reflectInt (unsafeCoerce unit :: IProxy i)


-- Addition
class SumInt (a :: Int) (b :: Int) (c :: Int) | a b -> c


-- (+ S a) + (+ b) = (+ S (a + b))
instance addPos :: (SumInt (Pos a) (Pos b) (Pos c')) =>  SumInt (Pos (Succ a)) (Pos b) (Pos (Succ c'))

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

plus :: ∀a b c. SumInt a b c => IProxy a -> IProxy b -> IProxy c
plus _ _ = unsafeCoerce unit :: IProxy c


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
else
instance inversePosSucc :: Inverse (Pos a) (Neg a)
else
instance inverseNegZ ∷ Inverse (Neg Z) (Pos Z) 
else
instance inverseNegSucc :: Inverse (Neg a) (Pos a)



-- Product

class ProductInt (a :: Int) (b :: Int) (c :: Int) | a b -> c

instance productPosNeg :: ProductInt (Pos a) (Pos b) (Pos c) => ProductInt (Neg a) (Pos b) (Neg c)

instance productNegNeg :: ProductInt (Pos a) (Pos b) (Pos c) => ProductInt (Neg a) (Neg b) (Pos c)

instance productZ :: ProductInt (Pos Z) a (Pos Z)
else
instance product1 :: ProductInt (Pos (Succ Z)) a a
else
instance productNegPos :: ProductInt (Pos a) (Pos b) (Pos c) => ProductInt (Pos a) (Neg b) (Neg c)
else
-- (1 + a) * b = b + (a * b)
instance productSucc :: (ProductInt (Pos a) b ab, SumInt ab b result) => ProductInt (Pos (Succ a)) b result

prod :: ∀a b c. ProductInt a b c => IProxy a -> IProxy b -> IProxy c
prod _ _ = unsafeCoerce unit :: IProxy c


-- Parsing

-- | Parse a Int from a Symbol 
-- |
-- | ```purescript
-- | ParseInt "-10" N10
-- | ParseInt "1337" P1337 -- P1137 would be type alias for Pos (Succ^1337 Z)
-- | ```
-- |
class ParseInt (sym :: Symbol) (int :: Int) | int -> sym, sym -> int

instance parseSigned :: 
   ( Equals "-" head isMinus
   , If isMinus (IProxy (Neg natValue)) (IProxy (Pos natValue)) (IProxy int)
   , If isMinus (SProxy tail) (SProxy sym) (SProxy numberSymbol)
   , Cons head tail sym
   , ParseNat numberSymbol natValue
   ) => ParseInt sym int

-- | parse Int a Value-Level
-- |
-- | ```purescript
-- | parseInt (undefined :: SProxy "-1337") N1337 
-- | 	-- N1137 would be type alias for Neg (Succ^1337 Z)
-- | ```
-- |
parseInt :: ∀a sym. ParseInt sym a => SProxy sym -> IProxy a
parseInt _ = unsafeCoerce unit