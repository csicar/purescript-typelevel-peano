module Test.Main where

import Prelude

import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Console (logShow)
import Type.Data.Peano (D0, D1, D10, D100, D2, D3, D4, D5, D6, D64, D9, N2, N4, Neg, P1, P10, P2, P4, P8, Pos, Succ, Z, d1, d2, mulNat, n1, p1, parseInt, parseNat, plus, plusNat, powNat, prod, reflectInt)
import Type.Data.Peano as Peano
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


two :: Proxy (Pos (Succ (Succ Z)))
two = Proxy

three :: Proxy (Pos (Succ (Succ (Succ Z))))
three = Proxy

minusTwo :: Proxy (Neg (Succ (Succ Z)))
minusTwo = Proxy

minusZero :: Proxy (Neg Z)
minusZero = Proxy

-- Test Add

t1 :: Proxy (Pos (Succ (Succ (Succ Z))))
t1 = p1 `plus` two

t1' :: Proxy (Pos (Succ (Succ (Succ Z))))
t1' = two `plus` p1

t2 :: Proxy (Neg (Succ (Succ (Succ Z))))
t2 = minusTwo `plus` n1

t2' :: Proxy (Neg (Succ (Succ (Succ Z))))
t2' = minusTwo `plus` n1

t3 :: Proxy (Neg (Succ Z))
t3 = minusTwo `plus` p1

t3' :: Proxy (Neg (Succ Z))
t3' = p1 `plus` minusTwo

t4 :: Proxy (Neg (Succ (Succ (Succ (Succ Z)))))
t4 = minusTwo `plus` minusTwo

t5 :: Proxy (Neg (Succ (Succ Z)))
t5 = t4 `plus` two

t5' :: Proxy (Neg (Succ (Succ Z)))
t5' = two `plus` t4

t6 :: Proxy (Neg (Succ (Succ Z)))
t6 = two `plus` (minusTwo `plus` minusTwo)

t6' :: Proxy (Neg (Succ (Succ Z)))
t6' = (minusTwo `plus` minusTwo) `plus` two

t7 :: Proxy (Pos Z)
t7 = n1 `plus` p1

t7' :: Proxy (Pos Z)
t7' = p1 `plus` n1

t8 :: Proxy P1
t8 = p1 `plus` minusZero

t9 :: Proxy P4
t9 = (p1 `plus` p1) `prod` (p1 `plus` p1)

t10 :: Proxy P8
t10 = t9 `prod` (p1 `plus` p1)

t11 :: Proxy (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
t11 = t10 `prod` t10

t12 :: Proxy (Pos (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
t12 = parseInt (SProxy :: SProxy "64")

t13 :: Proxy P10
t13 = parseInt (SProxy :: SProxy "10")

t14 :: Proxy N2
t14 = n1 `prod` two

t14' :: Proxy N2
t14' = two `prod` n1

t15 :: Proxy P4
t15 = minusTwo `prod` minusTwo

t16 :: Proxy N2
t16 = parseInt (SProxy :: SProxy "-2")

t17 :: Proxy N4
t17 = parseInt (SProxy :: SProxy "-4")

t18 :: Proxy D3
t18 = parseNat (SProxy :: SProxy "3")

t19 :: Proxy D9
t19 = parseNat (SProxy :: SProxy "9")

t20 :: Proxy D10
t20 = (Proxy :: Proxy D0) `plusNat` (Proxy :: Proxy D10)

t22 :: Proxy D10
t22 = plusNat (Proxy :: Proxy D5) (Proxy :: Proxy D5)

t23 :: Proxy D10
t23 = parseNat (SProxy :: SProxy "10")

t24 :: Proxy D10
t24 = (Proxy :: Proxy D10) `plusNat` (Proxy :: Proxy D0)

t24' :: Proxy D10
t24' = (Proxy :: Proxy D0) `plusNat` (Proxy :: Proxy D10)

t25 :: Proxy D4
t25 = (Proxy :: Proxy D2) `mulNat` (Proxy :: Proxy D2)

t26 :: Proxy D2
t26 = (Proxy :: Proxy D1) `mulNat` (Proxy :: Proxy D2)

t27 :: Proxy D64
t27 = (Proxy :: Proxy D2) `powNat` (Proxy :: Proxy D6)

t28 :: Proxy D100
t28 = parseNat (SProxy :: SProxy "100")

-- Test Proxy-agnostic
t29 :: forall (proxy :: Peano.Int -> Type). proxy P2
t29 = plus p1 p1

t30 :: forall (proxy :: Peano.Int -> Type). proxy P1
t30 = prod p1 p1

t31 :: forall (proxy :: Peano.Nat -> Type). proxy D2
t31 = plusNat d1 d1

t32 :: forall (proxy :: Peano.Nat -> Type). proxy D1
t32 = mulNat d1 d1

t33 :: forall (proxy :: Peano.Nat -> Type). proxy D4
t33 = powNat d2 d2

t34 :: forall (sproxy :: Symbol -> Type) (proxy :: Peano.Int -> Type). proxy P2
t34 = parseInt (unsafeCoerce unit :: sproxy "2")

t35 :: forall (sproxy :: Symbol -> Type) (proxy :: Peano.Int -> Type). proxy N2
t35 = parseInt (unsafeCoerce unit :: sproxy "-2")

t36 :: forall (sproxy :: Symbol -> Type) (proxy :: Peano.Nat -> Type). proxy D2
t36 = parseNat (unsafeCoerce unit :: sproxy "2")

main :: Effect Unit
main = do
  logShow t26
  logShow (reflectInt t12)
  logShow (Proxy :: Proxy D10)
  logShow t12
