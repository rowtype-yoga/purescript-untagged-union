{ name = "purescript-untagged-to-tagged"
, dependencies =
  [ "console", "effect", "either", "prelude", "psci-support", "untagged-union" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT-0"
, repository = "https://github.com/sigma-andex/purescript-typescript-importer.git"
}
