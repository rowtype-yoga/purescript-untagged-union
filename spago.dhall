{ name = "untagged-union"
, license = "MIT"
, repository = "https://github.com/jvliwanag/purescript-untagged-union.git"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "either"
  , "foreign"
  , "foreign-object"
  , "literals"
  , "maybe"
  , "newtype"
  , "prelude"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
