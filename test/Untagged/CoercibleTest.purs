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
  , undefOrStrOrNum :: UndefinedOr (String |+| Number)
  }


testCoerce :: Effect Unit
testCoerce = do

  -- urecord compile tests
  let pExplicitOneOf =
        coerce { str: "foo"
               , isb: (coerce 3 :: Int |+| String |+| Boolean)
               , undefOrNumber: (coerce 2.0 :: UndefinedOr Number)
               , undefOrStrOrNum: (coerce "bar" :: UndefinedOr (String |+| Number))
               } :: Props

  let pNoExplicitOneOf =
        coerce { str: "foo"
               , isb: "bar"
               , undefOrNumber: undefined
               , undefOrStrOrNum: 3.0
               } :: Props

  let pMixExplicitOneOf =
        coerce { str: "foo"
               , isb: "bar"
               , undefOrNumber: undefined
               , undefOrStrOrNum: (coerce "bar" :: UndefinedOr (String |+| Number))
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

--  let pWrongNumOrUndef =
--        coerce { str: "foo"
--               , undefOrNumber: "bar"
--               , undefOrStrOrNum: 3.0
--               } :: Props


  pure unit
