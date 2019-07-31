module Type.Data.Peano.Nat where

import Prelude (class Show, show, unit, ($), (+))
import Prim.Symbol as Symbol
import Type.Prelude (SProxy, kind Ordering, EQ, LT, GT)
import Unsafe.Coerce (unsafeCoerce)


-- Nat

foreign import kind Nat
foreign import data Z :: Nat
foreign import data Succ :: Nat -> Nat

data NProxy (n :: Nat)

type D0  = Z
type D1  = Succ Z
type D2  = Succ (Succ Z)
type D3  = Succ (Succ (Succ Z))
type D4  = Succ (Succ (Succ (Succ Z)))
type D5  = Succ (Succ (Succ (Succ (Succ Z))))
type D6  = Succ (Succ (Succ (Succ (Succ (Succ Z)))))
type D7  = Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))
type D8  = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))
type D9  = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))
type D10 = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))


class IsNat (a :: Nat) where
  reflectNat :: NProxy a -> Int

instance isNatZ ∷ IsNat Z where
  reflectNat _ = 0

instance isNatSucc ∷ IsNat a => IsNat (Succ a) where
  reflectNat _ = 1 + (reflectNat (unsafeCoerce unit :: NProxy a))

instance showZ ∷ IsNat a => Show (NProxy a) where
  show a = show $ reflectNat (unsafeCoerce unit :: NProxy a)


-- Addition

class SumNat (a :: Nat) (b :: Nat) (c :: Nat) | a b -> c

instance addZ ∷ SumNat a Z a
else
instance addZ' ∷ SumNat Z a a
else
instance addSucc :: SumNat a b c => SumNat (Succ a) b (Succ c)

plusNat :: ∀a b c. SumNat a b c => NProxy a -> NProxy b -> NProxy c
plusNat _ _ = unsafeCoerce unit :: NProxy c

-- Product

class ProductNat (a :: Nat) (b :: Nat) (c :: Nat) | a b -> c

--| 0 * a = 0
instance productZ' :: ProductNat Z a Z

--| 1 * a = a
instance product1' ∷ ProductNat (Succ Z) a a
else
--| (1 + a) * b = b + a * b
instance productSucc :: (ProductNat a b ab, SumNat ab b result) => ProductNat (Succ a) b result

mulNat :: ∀a b c. ProductNat a b c => NProxy a -> NProxy b -> NProxy c
mulNat _ _ = unsafeCoerce unit :: NProxy c

-- Compare

class CompareNat (a :: Nat) (b :: Nat) (ord :: Ordering) | a b -> ord

instance compareSame :: CompareNat a a EQ
else
instance compareZ :: CompareNat Z a LT
else
instance compareZ' ∷ CompareNat a Z GT
else
instance compareSucc ∷ CompareNat a b ord => CompareNat (Succ a) (Succ b) ord


-- Parse

class ParseNat (sym :: Symbol) (nat :: Nat) | nat -> sym, sym -> nat

instance parseLit0 :: ParseNat "0" Z
else
instance parseLit1 :: ParseNat "1" (Succ Z)
else
instance parseLit2 :: ParseNat "2" (Succ (Succ Z))
else
instance parseLit3 :: ParseNat "3" (Succ (Succ (Succ Z)))
else
instance parseLit4 :: ParseNat "4" (Succ (Succ (Succ (Succ Z))))
else
instance parseLit5 :: ParseNat "5" (Succ (Succ (Succ (Succ (Succ Z)))))
else
instance parseLit6 :: ParseNat "6" (Succ (Succ (Succ (Succ (Succ (Succ Z))))))
else
instance parseLit7 :: ParseNat "7" (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))
else
instance parseLit8 :: ParseNat "8" (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z))))))))
else
instance parseLit9 :: ParseNat "9" (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))
else
instance parseCons :: (ParseNat head msd, Symbol.Cons head tail sym, ProductNat msd D10 high, ParseNat tail lower, SumNat high lower res) => ParseNat sym res
-- instance parseCons :: (ParseNumber head msd, Symbol.Cons head tail sym, Product (Pos msd) P10 high, ParseNumber tail lower, Sum high (Pos lower) (Pos res)) => ParseNumber sym res

parseNat :: ∀a sym. ParseNat sym a => SProxy sym -> NProxy a
parseNat _ = unsafeCoerce unit