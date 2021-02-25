Purescript Typelevel Peano Numbers
==================================

Typelevel Numbers using Peano-Style Encoding.


### Installation

```bash
bower install purescript-typelevel-peano
```

### Documentation

Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-typelevel-peano/).

```purescript
> import Type.Data.Peano
> -- aliases for negative integers:
> :t n2
forall (proxy :: Int -> Type). proxy (Neg (Succ (Succ Z)))
> -- `proxy` can be either `IProxy` or `Proxy`

> -- and positive integers
> :t p2
forall (proxy :: Int -> Type). proxy (Pos (Succ (Succ Z)))

> -- based on natural numbers:
> :t d2
forall (proxy :: Int -> Type). proxy (Succ (Succ Z))
> -- `proxy` can be  either `NProxy` or `Proxy`

> -- parsing of arbitrary numbers from a symbol:
> :t parseInt (SProxy :: SProxy "-12")  -- or (Proxy :: Proxy "-12")
forall (proxy :: Int -> Type). proxy (Neg (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))))

> -- plus, prod for integers
> :t p2 `plus` n3
forall (proxy :: Int -> Type). proxy (Neg (Succ Z))

> :t p2 `prod` n3
forall (proxy :: Int -> Type). proxy (Neg (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))

> -- reflect to value level
> reflectInt p99
99 -- :: Int

> showInt p99
"99" -- :: String
```
