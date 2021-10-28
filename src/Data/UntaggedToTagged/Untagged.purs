module Data.UntaggedToTagged.Untagged where

import Prelude
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Sum(..), to)
import Untagged.TypeCheck (class HasRuntimeType)
import Untagged.Union (OneOf, toEither1)

class UntaggedHelper :: Type -> Type -> Constraint
class UntaggedHelper untagged tagged | untagged -> tagged where
  toTaggedHelper :: untagged -> tagged

instance
  ( HasRuntimeType l
  , UntaggedHelper (OneOf ln rn) next
  ) =>
  UntaggedHelper (OneOf l (OneOf ln rn)) (Sum (Constructor sym (Argument l)) next) where
  toTaggedHelper untagged = case toEither1 untagged of
    Left l -> Inl (Constructor (Argument l))
    Right r -> Inr $ toTaggedHelper r
else instance
  (HasRuntimeType l) =>
  UntaggedHelper (OneOf l r) (Sum (Constructor syml (Argument l)) (Constructor symr (Argument r))) where
  toTaggedHelper untagged = case toEither1 untagged of
    Left l -> Inl (Constructor (Argument l))
    Right r -> Inr (Constructor (Argument r))

class Untagged :: Type -> Type -> Constraint
class Untagged untagged tagged where
  -- | Convert an untagged union to a tagged union. E.g. 
  -- | 
  -- | ```purescript
  -- | type ISU = Int |+| String 
  -- | 
  -- | data IST = IT Int | ST String 
  -- | derive instance Generic IST _ 
  -- | 
  -- | isu :: ISU
  -- | isu = asOneOf "Wurst"
  -- | 
  -- | ist :: IST 
  -- | ist = toTagged isu
  -- | -- (ST "Wurst")
  -- | ```
  toTagged :: untagged -> tagged

instance (Generic tagged taggedGen, UntaggedHelper untagged taggedGen) => Untagged untagged tagged where
  toTagged = toTaggedHelper >>> to
