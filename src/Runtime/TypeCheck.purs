module Runtime.TypeCheck
       ( class HasRuntimeType
       , hasRuntimeType
       , newtypeHasRuntimeType
       , cast
       , jsTypeOf
       , hasJsType
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class HasRuntimeType a where
  hasRuntimeType :: forall x. Proxy a -> x -> Boolean

instance hasRuntimeTypeBoolean :: HasRuntimeType Boolean where
  hasRuntimeType _ = hasJsType "boolean"

instance hasRuntimeTypeInt :: HasRuntimeType Int where
  hasRuntimeType _ = isInt

instance hasRuntimeTypeNumber :: HasRuntimeType Number where
  hasRuntimeType _ = hasJsType "number"

instance hasRuntimeTypeString :: HasRuntimeType String where
  hasRuntimeType _ = hasJsType "string"

newtypeHasRuntimeType :: forall a r x. Newtype a r => HasRuntimeType r => Proxy a -> x -> Boolean
newtypeHasRuntimeType _ = hasRuntimeType (Proxy :: Proxy r)

foreign import jsTypeOf :: forall x. x -> String
foreign import isInt :: forall x. x -> Boolean

hasJsType :: forall x. String -> x -> Boolean
hasJsType name x =
  jsTypeOf x == name

cast :: forall a x. HasRuntimeType a => x -> Maybe a
cast x =
  if hasRuntimeTypeA x
  then Just (unsafeCoerce x)
  else Nothing

  where
    hasRuntimeTypeA = hasRuntimeType (Proxy :: Proxy a)
