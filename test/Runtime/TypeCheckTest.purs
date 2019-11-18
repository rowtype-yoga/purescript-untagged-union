module Runtime.TypeCheckTest
       ( testTypeCheck
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Foreign (Foreign)
import Foreign.Object as Foreign
import Runtime.TypeCheck (class HasRuntimeType, cast, hasRuntimeType, newtypeHasRuntimeType)
import Test.Assert (assertEqual, assertFalse, assertTrue)
import Type.Proxy (Proxy(..))

newtype MyString = MyString String
derive instance myStringNewtype :: Newtype MyString _
instance myStringHasRuntimeType :: HasRuntimeType MyString where
  hasRuntimeType = newtypeHasRuntimeType

testTypeCheck :: Effect Unit
testTypeCheck = do
  assertTrue (hasRuntimeType (Proxy :: _ Boolean) true)
  assertTrue (hasRuntimeType (Proxy :: _ Boolean) false)
  assertFalse (hasRuntimeType (Proxy :: _ Boolean) 0)

  assertTrue (hasRuntimeType (Proxy :: _ Int) 2)
  assertTrue (hasRuntimeType (Proxy :: _ Int) 2.0)
  assertFalse (hasRuntimeType (Proxy :: _ Int) "foo")

  assertTrue (hasRuntimeType (Proxy :: _ Number) 2.2)
  assertFalse (hasRuntimeType (Proxy :: _ Number) "foo")

  assertTrue (hasRuntimeType (Proxy :: _ String) "foo")
  assertFalse (hasRuntimeType (Proxy :: _ String) 2.0)

  assertTrue (hasRuntimeType (Proxy :: _ Foreign) "foo")

  assertFalse (hasRuntimeType (Proxy :: _ (Foreign.Object Foreign)) 2)
  assertTrue (hasRuntimeType (Proxy :: _ (Foreign.Object Foreign)) {i: 2})
  assertFalse (hasRuntimeType (Proxy :: _ (Foreign.Object String)) {i: 2})

  -- Newtypes
  assertTrue (hasRuntimeType (Proxy :: _ MyString) "foo")

  -- Cast
  assertEqual { actual: cast 2.0, expected: Just 2 }
  assertEqual { actual: cast "foo", expected: (Nothing :: Maybe Int) }
