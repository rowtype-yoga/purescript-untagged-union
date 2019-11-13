module OneOf
       ( OneOf
       , type (|+|)
       , class InOneOf
       , class RawType
       , isOfType
       , asOneOf
       , fromOneOf
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign (Foreign, unsafeToForeign)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data OneOf :: Type -> Type -> Type

infixr 7 type OneOf as |+|

class InOneOf a h t
instance inOneOfHead :: InOneOf a a t
else instance inOneOfLast :: InOneOf a h a
else instance inOneOfTail :: (InOneOf a h' t') => InOneOf a h (OneOf h' t')

asOneOf :: forall a h t. InOneOf a h t => a -> OneOf h t
asOneOf = unsafeCoerce

fromOneOf :: forall h t a. InOneOf a h t => RawType a => OneOf h t -> Maybe a
fromOneOf f =
  if isOfType (Proxy :: Proxy a) (unsafeToForeign f)
  then Just $ unsafeCoerce f
  else Nothing

class RawType a where
  isOfType :: Proxy a -> Foreign -> Boolean

instance rawTypeBoolean :: RawType Boolean where
  isOfType _ = isOfJsType "boolean"

instance rawTypeInt :: RawType Int where
  isOfType _ = isInt

instance rawTypeNumber :: RawType Number where
  isOfType _ = isOfJsType "number"

instance rawTypeString :: RawType String where
  isOfType _ = isOfJsType "string"

isOfJsType :: String -> Foreign -> Boolean
isOfJsType name f =
  jsTypeOf f == name

foreign import jsTypeOf :: Foreign -> String
foreign import isInt :: Foreign -> Boolean
