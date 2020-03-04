module Untagged.UnionTest
       ( testUnion
       ) where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Literals.Undefined (undefined)
import Test.Assert (assert, assertEqual, assertTrue)
import Untagged.Coercible (coerce)
import Untagged.Union (type (|+|), UndefinedOr, asOneOf, fromOneOf, fromUndefinedOr, getLeft, getLeft', getRight, getRight', maybeToUor, reduce, toEither1, uorToMaybe, withUor)

type ISB = Int |+| String |+| Boolean

type Props =
  { str :: String
  , isb :: ISB
  , undefOrNumber :: UndefinedOr Number
  , undefOrStrOrNum :: UndefinedOr (String |+| Number)
  }

testUnion :: Effect Unit
testUnion = do
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

  -- getLeft / getRight
  let sbString = asOneOf "foo" :: String |+| Boolean
  assertEqual
    { actual: getLeft sbString
    , expected: Just "foo"
    }
  assertEqual
    { actual: getLeft' sbString
    , expected: Just "foo"
    }
  assertEqual
    { actual: getRight sbString
    , expected: Nothing
    }
  assertEqual
    { actual: getRight' sbString
    , expected: Nothing
    }

  -- uor utils
  let soptStr = coerce "foo" :: UndefinedOr String
  let soptUndef = coerce undefined :: UndefinedOr String

  assertEqual
    { actual: uorToMaybe soptStr
    , expected: Just "foo"
    }
  assertEqual
    { actual: uorToMaybe soptUndef
    , expected: Nothing
    }

  assert $ maybeToUor (Just "foo") == soptStr
  assert $ maybeToUor Nothing == soptUndef

  assert $ withUor (_ <> "bar") soptStr == coerce "foobar"
  assert $ withUor (_ <> "bar") soptUndef == coerce undefined

  assertEqual
    { actual: fromUndefinedOr "baz" soptStr
    , expected: "foo"
    }
  assertEqual
    { actual: fromUndefinedOr "baz" soptUndef
    , expected: "baz"
    }

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

  log "done"
