module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import OneOf (type (|+|), asOneOf, fromOneOf)
import Test.Assert (assertEqual)

type ISB = Int |+| String |+| Boolean

main :: Effect Unit
main = do
  -- compile tests
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
