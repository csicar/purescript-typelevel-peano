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
IProxy (Neg (Succ (Succ Z)))

> -- and positive integers
> :t p2
IProxy (Pos (Succ (Succ Z)))

> -- based on natural numbers:
> :t d2
NProxy (Succ (Succ Z))

> -- parsing of arbitrary numbers from a symbol:
> :t parseInt (SProxy :: SProxy "-12")
IProxy (Neg (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))))))))

> -- plus, prod for integers
> :t p2 `plus` n3
IProxy (Neg (Succ Z))

> :t p2 `prod` n3
IProxy (Neg (Succ (Succ (Succ (Succ (Succ (Succ Z)))))))

> -- reflect to value level
> reflectInt p99
99 -- :: Int
```