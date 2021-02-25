module Type.Data.Peano.Int.Parse where

import Prelude (unit)

import Prim hiding (Int(..))

import Type.Data.Boolean (class If)
import Type.Data.Peano.Int.Definition (Neg, Pos, Int)
import Type.Data.Peano.Nat (class ParseNat)
import Type.Data.Symbol (class Cons, class Equals)
import Unsafe.Coerce (unsafeCoerce)

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
   , If isMinus (proxy (Neg natValue)) (proxy (Pos natValue)) (proxy int)
   , If isMinus (sproxy tail) (sproxy sym) (sproxy numberSymbol)
   , Cons head tail sym
   , ParseNat numberSymbol natValue
   ) => ParseInt sym int

-- | parse Int a Value-Level
-- |
-- | ```purescript
-- | parseInt (Proxy  :: _ "-1337") ~> N1337
-- | parseInt (SProxy :: _ "-1337") ~> N1337
-- | 	-- N1137 would be type alias for Neg (Succ^1337 Z)
-- | ```
-- |
parseInt :: ∀ sproxy proxy sym a. ParseInt sym a => sproxy sym -> proxy a
parseInt _ = unsafeCoerce unit
