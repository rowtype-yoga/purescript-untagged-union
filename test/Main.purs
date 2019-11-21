module Test.Main
       ( main
       ) where

import Prelude

import Effect (Effect)
import Untagged.UnionTest (testUnion)
import Untagged.TypeCheckTest (testTypeCheck)

main :: Effect Unit
main = do
  testTypeCheck
  testUnion
