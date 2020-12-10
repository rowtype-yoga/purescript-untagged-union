module Test.Sample where

import Data.Maybe (Maybe)
import Literals.Undefined (Undefined)
import Untagged.Castable (class Castable, cast)
import Untagged.Union (type (|+|), UndefinedOr, asOneOf, fromOneOf)

type ISB = Int |+| String |+| Boolean

isb1 :: ISB
isb1 = asOneOf 20

isb2 :: ISB
isb2 = asOneOf "foo"

--isb3 :: ISB
--isb3 = asOneOf 3.5

type OptionalInt = Undefined |+| Int

type OptionalInt' = UndefinedOr Int -- Same as `Undefined |+| Int`

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
  , fontSize :: Undefined |+| String |+| Number
  }

sampleProps :: Props
sampleProps =
  cast { text: "foo" -- text is required and should be a string

       , width: 30.0 -- width is optional, and may be defined, but should be a Number
         -- height is optional and may be omitted

       , fontSize: "100%" -- fontSize may be defined, and should either be a string or number
       }

-- Nested Unions

type ContainerProps =
  { titleProps :: UndefinedOr Props
  , opacity :: UndefinedOr Number
  }

sampleContainerProps :: ContainerProps
sampleContainerProps =
  cast { titleProps: (cast { text: "Foo" } :: Props)
       }

props :: forall r. Castable r Props => r -> Props
props = cast

containerProps :: forall r. Castable r ContainerProps => r -> ContainerProps
containerProps = cast

sampleContainerProps' :: ContainerProps
sampleContainerProps' =
  containerProps { titleProps: props { text: "Foo" }
                 }
