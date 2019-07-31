module Test.Main where

import Prelude

import Data.Symbol (SProxy)
import Effect (Effect)
import Effect.Console (log, logShow)
import Type.Data.Peano.Int (IProxy, N2, N4, Neg, P1, P10, P4, P8, Pos, n1, p1, parseInt, plus, prod, reflectInt)
import Type.Data.Peano.Nat (D0, D1, D10, D2, D3, D4, D5, D9, NProxy, Succ, Z, mulNat, parseNat, plusNat)
import Unsafe.Coerce (unsafeCoerce)

undefined :: ∀a. a
undefined = unsafeCoerce unit

two :: IProxy (Pos (Succ (Succ Z)))
two = undefined

three :: IProxy (Pos (Succ (Succ (Succ Z))))
three = undefined

minusTwo :: IProxy (Neg (Succ (Succ Z)))
minusTwo = undefined

minusZero :: IProxy (Neg Z)
minusZero = undefined

-- Test Add

t1 :: IProxy (Pos (Succ (Succ (Succ Z))))
t1 = p1 `plus` two

t1' :: IProxy (Pos (Succ (Succ (Succ Z))))
t1' = two `plus` p1

t2 :: IProxy (Neg (Succ (Succ (Succ Z))))
t2 = minusTwo `plus` n1

t2' :: IProxy (Neg (Succ (Succ (Succ Z))))
t2' = minusTwo `plus` n1

t3 :: IProxy (Neg (Succ Z))
t3 = minusTwo `plus` p1

t3' :: IProxy (Neg (Succ Z))
t3' = p1 `plus` minusTwo

t4 :: IProxy (Neg (Succ (Succ (Succ (Succ Z)))))
t4 = minusTwo `plus` minusTwo

t5 :: IProxy (Neg (Succ (Succ Z)))
t5 = t4 `plus` two

t5' :: IProxy (Neg (Succ (Succ Z)))
t5' = two `plus` t4

t6 :: IProxy (Neg (Succ (Succ Z)))
t6 = two `plus` (minusTwo `plus` minusTwo)

t6' :: IProxy (Neg (Succ (Succ Z)))
t6' = (minusTwo `plus` minusTwo) `plus` two

t7 :: IProxy (Pos Z)
t7 = n1 `plus` p1

t7' :: IProxy (Pos Z)
t7' = p1 `plus` n1

t8 :: IProxy P1
t8 = p1 `plus` minusZero

t9 :: IProxy P4
t9 = (p1 `plus` p1) `prod` (p1 `plus` p1)

t10 :: IProxy P8
t10 = t9 `prod` (p1 `plus` p1)

t11 :: IProxy (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
t11 = t10 `prod` t10

t12 :: IProxy (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
t12 = parseInt (undefined :: SProxy "64")

t13 :: IProxy P10
t13 = parseInt (undefined :: SProxy "10")

t14 :: IProxy N2
t14 = n1 `prod` two

t14' :: IProxy N2
t14' = two `prod` n1

t15 :: IProxy P4
t15 = minusTwo `prod` minusTwo

t16 :: IProxy N2
t16 = parseInt (undefined :: SProxy "-2")

t17 :: IProxy N4
t17 = parseInt (undefined :: SProxy "-4")

t18 :: NProxy D3
t18 = parseNat (undefined :: SProxy "3")

t19 :: NProxy D9
t19 = parseNat (undefined :: SProxy "9")

t20 :: NProxy D10
t20 = (undefined :: NProxy D0) `plusNat` (undefined :: NProxy D10)

t22 :: NProxy D10
t22 = plusNat (undefined :: NProxy D5) (undefined :: NProxy D5)

t23 :: NProxy D10
t23 = parseNat (undefined :: SProxy "10")

t24 :: NProxy D10
t24 = (undefined :: NProxy D10) `plusNat` (undefined :: NProxy D0)

t24' :: NProxy D10
t24' = (undefined :: NProxy D0) `plusNat` (undefined :: NProxy D10)

t25 :: NProxy D4
t25 = (undefined :: NProxy D2) `mulNat` (undefined :: NProxy D2)

t26 :: NProxy D2
t26 = (undefined :: NProxy D1) `mulNat` (undefined :: NProxy D2)


main :: Effect Unit
main = do
  logShow t26
  logShow (reflectInt t12)
  logShow (undefined :: NProxy D10)
  logShow t12
  log "You should add some tests."
