# purescript-oneof

A data type for untagged unions.

# Overview

Consider a the following type:

```purescript
import Untagged.Union

type ISB = Int |+| String |+| Boolean
```

The type `ISB` describes values which can either be an `Int`, a `String` or a `Boolean`. Moreover, it is guaranteed that at runtime, values of type `ISB` is directly stored as an `Int`, a `String` or a `Boolean` without any wrappers. This makes it especially useful for FFI.

Note that `|+|` is an alias for `OneOf`.

## Creating a `OneOf`

In order to create a value of `OneOf`, use `asOneOf`.

```purescript

isb1 :: ISB
isb1 = asOneOf 20

isb2 :: ISB
isb2 = asOneOf "foo"


--isb3 :: ISB
--isb3 = asOneOf 3.5

-- isb3 will fail since 3.5 is a Number which is neither
-- an Int, String nor Boolean

```

## Usage with Undefined

The library also defines `Undefined`. Combined with `OneOf`, it can represent an optional type:

```purescript
import Literals.Undefined

type OptionalInt = Int |+| Undefined
```

An alias `UndefinedOr` is also provided.

```purescript
type OptionalInt' = UndefinedOr Int -- Same as `Int |+| Undefined`
```

## Getting a value

To get a purescript value back, use `fromOneOf`:

```purescript
import Data.Maybe

valInt :: Maybe Int
valInt = fromOneOf isb1 -- evaluates to `Just 20`

valString :: Maybe String
valString = fromOneOf isb1 -- evalutes to `Nothing` since isb1 contains an Int

```

## Usage with records

Using `OneOf`, describing JS types as records becomes intuitive:

```purescript
type Props =
  { text :: String -- a required field

  -- Optional Fields
  , width :: UndefinedOr Number
  , height :: UndefinedOr Number

  -- Optional and Varying types
  , fontSize :: String |+| Number |+| Undefined
  }
```

A `coerce` helper is made available to convert records with the same runtime value:

```purescript
import Untagged.Coercible (coerce)

sampleProps :: Props
sampleProps =
  coerce { text: "foo" -- text is required and should be a string

         , width: 30.0 -- width is optional, and may be defined, but should be a Number
         -- height is optional and may be omitted
         , fontSize: "100%" -- fontSize may be defined, and should either be a string or number
         }

```
