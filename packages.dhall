let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.4-20191110/packages.dhall sha256:563a7f694e18e6399f7f6d01f5b7e3c3345781655d99945768f48e458feb93a4

let overrides = {=}

let additions =
      { literals =
        { dependencies =
          [ "assert"
          , "effect"
          , "console"
          , "integers"
          , "numbers"
          , "partial"
          , "psci-support"
          , "unsafe-coerce"
          , "typelevel-prelude"
          ]
        , repo = "https://github.com/jvliwanag/purescript-literals.git"
        , version = "7b2ae20f77c67b7e419a92fdd0dc7a09b447b18e"
        }
      }

in  upstream ⫽ overrides ⫽ additions
