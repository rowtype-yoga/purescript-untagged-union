module Untagged.CoercibleTest
       ( testCoerce
       ) where

import Prelude

import Effect (Effect)
import Literals.Undefined (undefined)
import Untagged.Coercible (coerce)
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
        coerce { str: "foo"
               , isb: (coerce 3 :: Int |+| String |+| Boolean)
               , undefOrNumber: (coerce 2.0 :: UndefinedOr Number)
               , undefOrISB: (coerce "bar" :: UndefinedOr (Int |+| String |+| Boolean))
               } :: Props

  let pNoExplicitOneOfI =
        coerce { str: "foo"
               , isb: "bar"
               , undefOrNumber: undefined
               , undefOrISB: 3
               } :: Props

  let pNoExplicitOneOfS =
        coerce { str: "foo"
               , isb: "bar"
               , undefOrNumber: undefined
               , undefOrISB: "foo"
               } :: Props

  let pNoExplicitOneOfB =
        coerce { str: "foo"
               , isb: "bar"
               , undefOrNumber: undefined
               , undefOrISB: true
               } :: Props


  let pMixExplicitOneOf =
        coerce { str: "foo"
               , isb: "bar"
               , undefOrNumber: undefined
               , undefOrISB: (coerce "bar" :: UndefinedOr (Int |+| String |+| Boolean))
               } :: Props

  let pOmitOptional =
        coerce { str: "foo"
               , isb: "bar"
               } :: Props


  -- should not compile
--  let pMissingStr =
--       coerce { isb: "bar"
--              , undefOrNumber: undefined
--              , undefOrStrOrNum: 3.0
--              } :: Props

--  let pMissingIsb =
--        coerce { str: "foo"
--               , undefOrNumber: undefined
--               , undefOrStrOrNum: 3.0
--               } :: Props

--  let pWrongUndefOrNumber =
--        coerce { str: "foo"
--               , undefOrNumber: "bar"
--               , undefOrISB: 3
--               } :: Props


  pure unit
