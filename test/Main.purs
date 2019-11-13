module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import OneOf (type (|+|), Undefined, asOneOf, fromOneOf, undefined, urecord)
import Test.Assert (assertEqual)

type ISB = Int |+| String |+| Boolean

type Props =
  { str :: String
  , isb :: ISB
  , numOrUndef :: Number |+| Undefined
  , strOrNumOrUndef :: String |+| Number |+| Undefined
  }

main :: Effect Unit
main = do
  -- asOneOf compile tests
  let isbInt = asOneOf 20 :: ISB
  let isbString = asOneOf "foo" :: ISB

  -- should not compile
  --let isbNumber = asOneOf 3.5 :: ISB

  assertEqual
    { actual: fromOneOf isbInt
    , expected: Just 20
    }

  assertEqual
    { actual: fromOneOf isbInt
    , expected: (Nothing :: Maybe String)
    }

  assertEqual
    { actual: fromOneOf isbString
    , expected: Just "foo"
    }

  assertEqual
    { actual: fromOneOf isbString
    , expected: (Nothing :: Maybe Int)
    }

  -- urecord compile tests
  let pExplicitOneOf =
        urecord { str: "foo"
                , isb: (asOneOf 3 :: ISB)
                , numOrUndef: (asOneOf 2.0 :: Number |+| Undefined)
                , strOrNumOrUndef: (asOneOf "bar" :: String |+| Number |+| Undefined)
                } :: Props

  let pNoExplicitOneOf =
        urecord { str: "foo"
                , isb: "bar"
                , numOrUndef: undefined
                , strOrNumOrUndef: 3.0
                } :: Props

  let pMixExplicitOneOf =
        urecord { str: "foo"
                , isb: "bar"
                , numOrUndef: undefined
                , strOrNumOrUndef: (asOneOf "bar" :: String |+| Number |+| Undefined)
                } :: Props

  let pOmitOptional =
        urecord { str: "foo"
                , isb: "bar"
                } :: Props


  -- should not compile
--  let pMissingStr =
--       urecord { isb: "bar"
--               , numOrUndef: undefined
--               , strOrNumOrUndef: 3.0
--               } :: Props

--  let pMissingIsb =
--        urecord { str: "foo"
--                , numOrUndef: undefined
--                , strOrNumOrUndef: 3.0
--                } :: Props

--  let pWrongNumOrUndef =
--        urecord { str: "foo"
--                , numOrUndef: "bar"
--                , strOrNumOrUndef: 3.0
--                } :: Props


  log "done"
