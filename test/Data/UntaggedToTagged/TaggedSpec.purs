module Data.UntaggedToTagged.TaggedSpec where

import Prelude
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UntaggedToTagged.Tagged (fromTagged)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Untagged.Union (type (|+|), asOneOf, toEither1)

type ISU
  = Int |+| String |+| Boolean

newtype NTISU
  = NTISU ISU
instance Newtype NTISU ISU
derive newtype instance Eq NTISU

instance Show NTISU where
  show (NTISU isu) = case toEither1 isu of
    Left i -> show i
    Right sb -> case toEither1 sb of
      Left s -> s
      Right b -> show b

data IST
  = IT Int
  | ST String
  | BT Boolean

derive instance Generic IST _
instance Show IST where
  show = genericShow
instance Eq IST where
  eq = genericEq

spec :: Spec Unit
spec =
  describe "Data.UntaggedToTagged" do
    describe "fromTagged" do
      it "should convert a tagged to an untagged union" do
        let
          isul :: ISU
          isul = asOneOf 10

          isurl :: ISU
          isurl = asOneOf "Wurst"

          isurr :: ISU
          isurr = asOneOf true
        (NTISU $ fromTagged (IT 10)) `shouldEqual` (NTISU isul)
        (NTISU $ fromTagged (ST "Wurst")) `shouldEqual` (NTISU isurl)
        (NTISU $ fromTagged (BT true)) `shouldEqual` (NTISU isurr)
