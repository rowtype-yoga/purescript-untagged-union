module Runtime.OneOfTest
       ( testOneOf
       ) where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Runtime.OneOf (type (|+|), asOneOf, fromOneOf, toEither1, reduce, urecord)
import Runtime.Undefined (Undefined, undefined)
import Test.Assert (assertEqual, assertTrue)

type ISB = Int |+| String |+| Boolean

type Props =
  { str :: String
  , isb :: ISB
  , numOrUndef :: Number |+| Undefined
  , strOrNumOrUndef :: String |+| Number |+| Undefined
  }

testOneOf :: Effect Unit
testOneOf = do
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

  -- Eq instance
  assertTrue (isbInt == isbInt)
  assertTrue (isbInt /= asOneOf 100)
  assertTrue (isbString /= asOneOf 100)

  -- toEither1
  assertTrue (toEither1 isbInt == (Left 20 :: Either Int (String |+| Boolean)))
  assertTrue (toEither1 isbString == (Right (asOneOf "foo")))

  -- left bias:
  assertTrue (isLeft $ toEither1 (asOneOf 3 :: Int |+| Int))
  assertTrue (isLeft $ toEither1 (asOneOf 3.0 :: Int |+| Number))

  -- reduce
  let
    reduceISB =
        reduce ( (\(i :: Int) -> "i" <> show i)
                 /\ (\(s :: String) -> "s" <> s)
                 /\ (\(b :: Boolean) -> "b" <> show b)
               )
  assertEqual
    { actual: reduceISB isbInt
    , expected: "i20"
    }
  assertEqual
    { actual: reduceISB isbString
    , expected: "sfoo"
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
