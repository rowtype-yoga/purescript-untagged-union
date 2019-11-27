module Untagged.CoercibleTest
       ( testCoerce
       ) where

import Prelude

import Effect (Effect)
import Untagged.Coercible (coerce)
import Untagged.Union (type (|+|))
import Literals.Undefined (Undefined, undefined)

type Props =
  { str :: String
  , isb :: Int |+| String |+| Boolean
  , numOrUndef :: Number |+| Undefined
  , strOrNumOrUndef :: String |+| Number |+| Undefined
  }


testCoerce :: Effect Unit
testCoerce = do

  -- urecord compile tests
  let pExplicitOneOf =
        coerce { str: "foo"
               , isb: (coerce 3 :: Int |+| String |+| Boolean)
               , numOrUndef: (coerce 2.0 :: Number |+| Undefined)
               , strOrNumOrUndef: (coerce "bar" :: String |+| Number |+| Undefined)
               } :: Props

  let pNoExplicitOneOf =
        coerce { str: "foo"
               , isb: "bar"
               , numOrUndef: undefined
               , strOrNumOrUndef: 3.0
               } :: Props

  let pMixExplicitOneOf =
        coerce { str: "foo"
               , isb: "bar"
               , numOrUndef: undefined
               , strOrNumOrUndef: (coerce "bar" :: String |+| Number |+| Undefined)
               } :: Props

  let pOmitOptional =
        coerce { str: "foo"
               , isb: "bar"
               } :: Props


  -- should not compile
--  let pMissingStr =
--       coerce { isb: "bar"
--              , numOrUndef: undefined
--              , strOrNumOrUndef: 3.0
--              } :: Props

--  let pMissingIsb =
--        coerce { str: "foo"
--               , numOrUndef: undefined
--               , strOrNumOrUndef: 3.0
--               } :: Props

--  let pWrongNumOrUndef =
--        coerce { str: "foo"
--               , numOrUndef: "bar"
--               , strOrNumOrUndef: 3.0
--               } :: Props


  pure unit
