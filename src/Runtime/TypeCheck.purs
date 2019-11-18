module Runtime.TypeCheck
       ( class HasRuntimeType
       , hasRuntimeType
       , newtypeHasRuntimeType
       , cast
       , hasJsType
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
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

instance hasRuntimeTypeForeign :: HasRuntimeType Foreign where
  hasRuntimeType _ _ = true

instance hasRuntimeTypeObject :: HasRuntimeType e => HasRuntimeType (Object e) where
  hasRuntimeType _ x =
    hasJsType "object" x && (Object.all \_ -> hasRuntimeTypeE) (unsafeCoerce x)
    where
      hasRuntimeTypeE = hasRuntimeType (Proxy :: _ e)

newtypeHasRuntimeType :: forall a r x. Newtype a r => HasRuntimeType r => Proxy a -> x -> Boolean
newtypeHasRuntimeType _ = hasRuntimeType (Proxy :: Proxy r)

foreign import isInt :: forall x. x -> Boolean
foreign import jsTypeOf :: forall x. x -> String

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
