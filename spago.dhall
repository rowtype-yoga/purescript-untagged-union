{ name = "untagged-union"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "literals"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "proxy"
  , "psci-support"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
