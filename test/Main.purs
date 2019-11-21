module Test.Main
       ( main
       ) where

import Prelude

import Effect (Effect)
import Untagged.OneOfTest (testOneOf)
import Untagged.TypeCheckTest (testTypeCheck)

main :: Effect Unit
main = do
  testTypeCheck
  testOneOf
