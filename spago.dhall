{ name = "untagged-to-tagged"
, dependencies =
  [ "aff"
  , "effect"
  , "either"
  , "newtype"
  , "prelude"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "untagged-union"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT-0"
, repository =
    "https://github.com/sigma-andex/purescript-untagged-to-tagged.git"
}
