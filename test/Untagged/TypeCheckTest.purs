module Untagged.TypeCheckTest
       ( testTypeCheck
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object as Foreign
import Test.Assert (assertEqual, assertFalse, assertTrue)
import Type.Proxy (Proxy(..))
import Untagged.TypeCheck (class HasRuntimeType, cast, hasRuntimeType, newtypeHasRuntimeType)
import Untagged.Undefined (Undefined, undefined)

newtype MyString = MyString String
derive instance myStringNewtype :: Newtype MyString _
instance myStringHasRuntimeType :: HasRuntimeType MyString where
  hasRuntimeType = newtypeHasRuntimeType

testTypeCheck :: Effect Unit
testTypeCheck = do
  assertTrue (hasRuntimeTypeF (Proxy :: _ Undefined) undefined)
  assertFalse (hasRuntimeTypeF (Proxy :: _ Undefined) true)

  assertTrue (hasRuntimeTypeF (Proxy :: _ Boolean) true)
  assertTrue (hasRuntimeTypeF (Proxy :: _ Boolean) false)
  assertFalse (hasRuntimeTypeF (Proxy :: _ Boolean) 0)

  assertTrue (hasRuntimeTypeF (Proxy :: _ Int) 2)
  assertTrue (hasRuntimeTypeF (Proxy :: _ Int) 2.0)
  assertFalse (hasRuntimeTypeF (Proxy :: _ Int) "foo")

  assertTrue (hasRuntimeTypeF (Proxy :: _ Number) 2.2)
  assertFalse (hasRuntimeTypeF (Proxy :: _ Number) "foo")

  assertTrue (hasRuntimeTypeF (Proxy :: _ String) "foo")
  assertFalse (hasRuntimeTypeF (Proxy :: _ String) 2.0)

  assertTrue (hasRuntimeTypeF (Proxy :: _ Foreign) "foo")

  assertTrue (hasRuntimeTypeF (Proxy :: _ (Foreign.Object Foreign)) {i: 2})
  assertFalse (hasRuntimeTypeF (Proxy :: _ (Foreign.Object Foreign)) 2)
  assertFalse (hasRuntimeTypeF (Proxy :: _ (Foreign.Object String)) {i: 2})

  assertTrue (hasRuntimeTypeF (Proxy :: _ {i :: Int, s :: MyString}) {i: 2.0, s: "foo"})
  assertTrue (hasRuntimeTypeF (Proxy :: _ {i :: Int}) sampleWithInherited)
  assertFalse (hasRuntimeTypeF (Proxy :: _ {i :: Int}) 5)
  -- should allow for extra members
  assertTrue (hasRuntimeTypeF (Proxy :: _ {i :: Int}) {i: 2.0, s: "foo"})

  -- Newtypes
  assertTrue (hasRuntimeTypeF (Proxy :: _ MyString) "foo")

  -- Cast
  assertEqual { actual: cast 2.0, expected: Just 2 }
  assertEqual { actual: cast "foo", expected: (Nothing :: Maybe Int) }

foreign import sampleWithInherited :: Foreign

hasRuntimeTypeF :: forall a x. HasRuntimeType a => Proxy a -> x -> Boolean
hasRuntimeTypeF p x = hasRuntimeType p (unsafeToForeign x)
