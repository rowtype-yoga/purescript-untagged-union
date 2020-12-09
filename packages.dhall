let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201206/packages.dhall sha256:c9ffd7577fb8ee2197309591d7ccc0f506ee37b9078866f0ef159f5abbb1b32b

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
