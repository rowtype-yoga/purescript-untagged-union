module Data.UntaggedToTagged.Tagged where

import Prelude
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Sum(..), from)
import Untagged.Union (class InOneOf, OneOf, asOneOf)

class TaggedHelper :: Type -> Type -> Constraint
class TaggedHelper taggedGen untagged | taggedGen -> untagged where
  fromTaggedHelper :: taggedGen -> untagged

instance
  (InOneOf r l r) =>
  TaggedHelper (Sum (Constructor syml (Argument l)) (Constructor symr (Argument r))) (OneOf l r) where
  fromTaggedHelper (Inl (Constructor (Argument l))) = asOneOf l
  fromTaggedHelper (Inr (Constructor (Argument r))) = asOneOf r
else instance
  ( InOneOf (OneOf ln rn) l (OneOf ln rn)
  , TaggedHelper next (OneOf ln rn)
  ) =>
  TaggedHelper (Sum (Constructor sym (Argument l)) next) (OneOf l (OneOf ln rn)) where
  fromTaggedHelper (Inl (Constructor (Argument l))) = asOneOf l
  fromTaggedHelper (Inr r) = asOneOf $ fromTaggedHelper r

class Tagged :: Type -> Type -> Constraint
class Tagged tagged untagged where
  -- | Convert a tagged union to an untagged union. E.g. 
  -- | data IST = IT Int | ST String 
  -- | derive instance Generic IST _ 
  -- | 
  -- | type ISU = Int |+| String 
  -- | 
  -- | ist :: IST
  -- | ist = ST "Wurst"
  -- | 
  -- | isu :: ISU 
  -- | isu = fromTagged isu
  -- | -- asOneOf "Wurst"
  fromTagged :: tagged -> untagged

instance (Generic tagged taggedGen, TaggedHelper taggedGen untagged) => Tagged tagged untagged where
  fromTagged = from >>> fromTaggedHelper
