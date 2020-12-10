module Untagged.CastableTest
       ( testCoerce
       ) where

import Prelude

import Effect (Effect)
import Literals.Undefined (undefined)
import Untagged.Castable (cast)
import Untagged.Union (type (|+|), UndefinedOr)

type Props =
  { str :: String
  , isb :: Int |+| String |+| Boolean
  , undefOrNumber :: UndefinedOr Number
  , undefOrISB :: UndefinedOr (Int |+| String |+| Boolean)
  }

testCoerce :: Effect Unit
testCoerce = do

  -- urecord compile tests
  let pExplicitOneOf =
        cast { str: "foo"
             , isb: (cast 3 :: Int |+| String |+| Boolean)
             , undefOrNumber: (cast 2.0 :: UndefinedOr Number)
             , undefOrISB: (cast "bar" :: UndefinedOr (Int |+| String |+| Boolean))
             } :: Props

  let pNoExplicitOneOfI =
        cast { str: "foo"
             , isb: "bar"
             , undefOrNumber: undefined
             , undefOrISB: 3
             } :: Props

  let pNoExplicitOneOfS =
        cast { str: "foo"
             , isb: "bar"
             , undefOrNumber: undefined
             , undefOrISB: "foo"
             } :: Props

  let pNoExplicitOneOfB =
        cast { str: "foo"
             , isb: "bar"
             , undefOrNumber: undefined
             , undefOrISB: true
             } :: Props

  let pMixExplicitOneOf =
        cast { str: "foo"
             , isb: "bar"
             , undefOrNumber: undefined
             , undefOrISB: (cast "bar" :: UndefinedOr (Int |+| String |+| Boolean))
             } :: Props

  let pOmitOptional =
        cast { str: "foo"
             , isb: "bar"
             } :: Props


  -- should not compile
--  let pMissingStr =
--       cast { isb: "bar"
--            , undefOrNumber: undefined
--            , undefOrStrOrNum: 3.0
--            } :: Props

--  let pMissingIsb =
--        cast { str: "foo"
--             , undefOrNumber: undefined
--             , undefOrStrOrNum: 3.0
--             } :: Props

--  let pWrongUndefOrNumber =
--        cast { str: "foo"
--             , undefOrNumber: "bar"
--             , undefOrISB: 3
--             } :: Props

  pure unit
