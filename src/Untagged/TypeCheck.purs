module Untagged.TypeCheck
       ( class HasRuntimeType
       , hasRuntimeType
       , newtypeHasRuntimeType
       , class HasRuntimeTypeRecordRL
       , hasRuntimeTypeRecRL
       , cast
       ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign (Foreign, isNull, typeOf, unsafeToForeign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Literals.Null (Null)
import Literals.Undefined (Undefined)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class HasRuntimeType :: forall k. k -> Constraint
class HasRuntimeType a where
  hasRuntimeType :: Proxy a -> Foreign -> Boolean

instance hasRuntimeTypeUndefined :: HasRuntimeType Undefined where
  hasRuntimeType _ = hasJsType "undefined"

instance hasRuntimeTypeNull :: HasRuntimeType Null where
  hasRuntimeType _ = isNull

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

instance hasRuntimeTypeRecord ::
  ( RowToList r rl
  , HasRuntimeTypeRecordRL rl
  ) => HasRuntimeType {|r} where
  hasRuntimeType _ x = hasJsType "object" x && hasRuntimeTypeRecRL (Proxy :: _ rl) (unsafeToForeign x)

class HasRuntimeTypeRecordRL (rl :: RowList Type) where
  hasRuntimeTypeRecRL :: forall proxy. proxy rl -> Foreign -> Boolean

instance hasRuntimeTypeRecordRLNil :: HasRuntimeTypeRecordRL Nil where
  hasRuntimeTypeRecRL _ _ = true

instance hasRuntimeTypeRecordRLCons ::
  ( HasRuntimeTypeRecordRL tl
  , HasRuntimeType a
  , IsSymbol sym
  ) => HasRuntimeTypeRecordRL (Cons sym a tl) where
  hasRuntimeTypeRecRL _ x = hasRuntimeTypeA property && hasRuntimeTypeRecRL tlProxy x
    where
      hasRuntimeTypeA = hasRuntimeType (Proxy :: _ a)
      propertyName = reflectSymbol (Proxy :: _ sym)
      property = getProperty propertyName x
      tlProxy = Proxy :: _ tl

foreign import getProperty :: String -> Foreign -> Foreign

newtypeHasRuntimeType :: forall a r. Newtype a r => HasRuntimeType r => Proxy a -> Foreign -> Boolean
newtypeHasRuntimeType _ = hasRuntimeType (Proxy :: Proxy r)

foreign import isInt :: forall x. x -> Boolean

hasJsType :: forall x. String -> x -> Boolean
hasJsType name x =
  typeOf (unsafeCoerce x) == name

cast :: forall a x. HasRuntimeType a => x -> Maybe a
cast x =
  if hasRuntimeTypeA (unsafeToForeign x)
  then Just (unsafeCoerce x)
  else Nothing

  where
    hasRuntimeTypeA = hasRuntimeType (Proxy :: Proxy a)
