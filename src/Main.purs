module Main where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), Sum(..), from, to)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Type.Proxy (Proxy(..))
import Untagged.TypeCheck (class HasRuntimeType)
import Untagged.Union (type (|+|), OneOf, asOneOf, toEither1)

class UntaggedHelper :: Type -> Type -> Constraint
class UntaggedHelper untagged tagged where 
  toTaggedHelper :: untagged -> tagged 

{-
(Inl (Constructor @"A6" (Argument "Wurst")))
UntaggedHelper (OneOf ln2 rn3)
                        (Sum (Constructor sym4 (Argument l5)) next6)
-}
instance (HasRuntimeType l, UntaggedHelper (OneOf ln rn) (Sum (Constructor sym (Argument l)) next)) => UntaggedHelper (OneOf l (OneOf ln rn)) (Sum (Constructor sym (Argument l)) next) where 
  toTaggedHelper untagged = case toEither1 untagged of 
    Left l -> Inl (Constructor (Argument l))
    Right r -> toTaggedHelper r 
    
else instance (HasRuntimeType l) => UntaggedHelper (OneOf l r) (Sum (Constructor syml (Argument l)) (Constructor symr (Argument r))) where
  toTaggedHelper untagged = case toEither1 untagged of 
    Left l -> Inl (Constructor (Argument l))
    Right r -> Inr (Constructor (Argument r))

class Untagged :: Type -> Type -> Constraint
class Untagged untagged tagged where 
  toTagged :: untagged -> tagged 

instance (Generic tagged taggedGen, UntaggedHelper untagged taggedGen) => Untagged untagged tagged where 
  toTagged = toTaggedHelper >>> to

type ISU = Int |+| String 

data IST = IT Int | ST String 
derive instance Generic IST _ 
instance Show IST where 
  show = genericShow

main :: Effect Unit
main = do
  let 
      
    isul :: ISU
    isul = asOneOf 10

    isur :: ISU
    isur = asOneOf "Wurst"

    istl :: IST 
    istl = toTagged isul

    istr :: IST 
    istr = toTagged isur
    
  log $ show istl
  log $ show istr
