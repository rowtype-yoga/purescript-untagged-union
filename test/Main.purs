module Test.Main
       ( main
       ) where

import Prelude

import Effect (Effect)
import Runtime.OneOfTest (testOneOf)
import Runtime.TypeCheckTest (testTypeCheck)

main :: Effect Unit
main = do
  testTypeCheck
  testOneOf
