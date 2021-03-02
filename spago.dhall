{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-typelevel-peano"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "prelude"
  , "psci-support"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, license = "MIT"
, repository = "https://github.com/csicar/purescript-typelevel-peano"
, sources = [ "src/**/*.purs" ]
}
