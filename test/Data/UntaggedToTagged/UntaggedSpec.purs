module Data.UntaggedToTagged.UntaggedSpec where

import Prelude
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.UntaggedToTagged (toTagged)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Untagged.Union (type (|+|), asOneOf)

type ISU
  = Int |+| String

data IST
  = IT Int
  | ST String

derive instance Generic IST _
instance Show IST where
  show = genericShow
instance Eq IST where
  eq = genericEq

spec :: Spec Unit
spec =
  describe "Data.UntaggedToTagged" do
    describe "toTagged" do
      it "should convert a untagged to a tagged union" do
        let
          isul :: ISU
          isul = asOneOf 10

          isur :: ISU
          isur = asOneOf "Wurst"
        (toTagged isul :: IST) `shouldEqual` (IT 10)
        (toTagged isur :: IST) `shouldEqual` (ST "Wurst")
