{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "oneof"
, dependencies =
    [ "assert"
    , "console"
    , "effect"
    , "foreign"
    , "maybe"
    , "proxy"
    , "psci-support"
    , "tuples"
    , "unsafe-coerce"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
