module Type.Data.Peano.Nat.Parse where

import Prelude (unit)
import Type.Data.Peano.Nat.Definition (class ProductNat, class SumNat, class ExponentiationNat, Succ, Z, Nat)
import Type.Data.Peano.Nat.TypeAliases (D10)

import Data.Symbol (SProxy)
import Prim (Symbol)
import Prim.Symbol as Symbol
import Unsafe.Coerce (unsafeCoerce)


class Length (sym :: Symbol) (nat :: Nat) | sym -> nat

instance length0 :: Length "" Z
else
instance lengthCons :: (Symbol.Cons head tail sym, Length tail tailLength) => Length sym (Succ tailLength)

length :: ∀ proxy a b. Length a b => SProxy a -> proxy b
length _ = unsafeCoerce unit


-- Parse

-- | Parses a Nat from a Symbol
-- |
-- | ```purescript
-- | ParseNat "2" ~> (Succ (Succ Z))
-- | ParseNat "1283" ~> (Succ (...))
-- | ```
-- |
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
-- head : tail
-- 
instance parseCons :: 
  ( ParseNat head msd
  , Symbol.Cons head tail sym
  , Length tail symLength
  , ExponentiationNat D10 symLength offset
  , ProductNat offset msd high
  , ParseNat tail lower
  , SumNat high lower res
  ) => ParseNat sym res


-- | value-level parse of number
-- |
-- | ```purescript
-- | parseNat (SProxy "10") ~> D10
-- | ```
-- |
parseNat :: ∀a sym proxy. ParseNat sym a => SProxy sym -> proxy a
parseNat _ = unsafeCoerce unit
