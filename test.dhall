let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs", "doctest/**/*.purs" ],
  dependencies = conf.dependencies # [ "spec" ]
}