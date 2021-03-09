{ name = "untagged-union"
, license = "MIT"
, repository = "https://github.com/jvliwanag/purescript-untagged-union.git"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "literals"
  , "maybe"
  , "newtype"
  , "psci-support"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
