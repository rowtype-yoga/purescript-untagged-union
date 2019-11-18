module Test.Sample where

import Runtime.OneOf (type (|+|), Undefined, UndefinedOr, asOneOf, fromOneOf, urecord)
import Data.Maybe (Maybe)

type ISB = Int |+| String |+| Boolean

isb1 :: ISB
isb1 = asOneOf 20

isb2 :: ISB
isb2 = asOneOf "foo"

--isb3 :: ISB
--isb3 = asOneOf 3.5

type OptionalInt = Int |+| Undefined

type OptionalInt' = UndefinedOr Int -- Same as `Int |+| Undefined`

valInt :: Maybe Int
valInt = fromOneOf isb1 -- evaluates to `Just 20`

valString :: Maybe String
valString = fromOneOf isb1 -- evalutes to `Nothing` since isb1 contains an Int

type Props =
  { text :: String -- a required field

  -- Optional Fields
  , width :: UndefinedOr Number
  , height :: UndefinedOr Number

  -- Optional and Varying types
  , fontSize :: String |+| Number |+| Undefined
  }

sampleProps :: Props
sampleProps =
  urecord { text: "foo" -- text is required and should be a string

          , width: 30.0 -- width is optional, and may be defined, but should be a Number
          -- height is optional and may be omitted

          , fontSize: "100%" -- fontSize may be defined, and should either be a string or number
          }
