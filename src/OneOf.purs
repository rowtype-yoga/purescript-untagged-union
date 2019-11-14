module OneOf
       ( OneOf
       , type (|+|)
       , class InOneOf
       , class RawType
       , isOfType
       , Undefined
       , undefined
       , UndefinedOr
       , class HasUndefined
       , asOneOf
       , fromOneOf
       , toEither1
       -- Record helpers
       , class CoercibleRecord
       , class CoercibleRecordRL
       , urecord
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Foreign (Foreign, unsafeToForeign)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data OneOf :: Type -> Type -> Type

instance oneOfEq :: (Eq a, Eq b, RawType a, RawType b) => Eq (OneOf a b) where
  eq o o' = case toEither1 o, toEither1 o' of
    Left a, Left a' -> a == a'
    Right b, Right b' -> b == b'
    _, _ -> false

infixr 7 type OneOf as |+|

class InOneOf a h t
instance inOneOfHead :: InOneOf a a t
else instance inOneOfLast :: InOneOf a h a
else instance inOneOfTail :: (InOneOf a h' t') => InOneOf a h (OneOf h' t')

foreign import data Undefined :: Type
foreign import undefined :: Undefined

type UndefinedOr a = OneOf a Undefined

class HasUndefined h t
instance hasUndefinedInstance :: InOneOf Undefined h t => HasUndefined h t

asOneOf :: forall a h t. InOneOf a h t => a -> OneOf h t
asOneOf = unsafeCoerce

fromOneOf :: forall h t a. InOneOf a h t => RawType a => OneOf h t -> Maybe a
fromOneOf f =
  if isOfType (Proxy :: Proxy a) (unsafeToForeign f)
  then Just $ unsafeCoerce f
  else Nothing

--| Unwraps a single layer of `OneOf` to an Either
--| Note that for some `x :: a |+| b`. If the value `x` has a runtime
--| value that can be read as either types `a` and `b`, then
--| `toEither1 x` will return `Left`.
--|
--| Example: toEither1 (asOneOf 3.0 :: Int |+| Number) == Left 3
toEither1 :: forall a b. RawType a => RawType b => OneOf a b -> Either a b
toEither1 o =
  if isTypeA o
  then Left (unsafeCoerce o)
  else Right (unsafeCoerce o)
  where
    isTypeA = isOfType (Proxy :: Proxy a) <<< unsafeToForeign

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

instance rawTypeUndefined :: RawType Undefined where
  isOfType _ = isOfJsType "undefined"

instance rawTypeOneOf :: (RawType a, RawType a') => RawType (OneOf a a') where
  isOfType _ = isOfType (Proxy :: Proxy a) || isOfType (Proxy :: Proxy a')

isOfJsType :: String -> Foreign -> Boolean
isOfJsType name f =
  jsTypeOf f == name

foreign import jsTypeOf :: Foreign -> String
foreign import isInt :: Foreign -> Boolean

urecord :: forall r r'. CoercibleRecord r r' => {|r} -> {|r'}
urecord = unsafeCoerce

class CoercibleRecord (r :: #Type) (r' :: #Type)

instance coercibleRecordInstance ::
  ( RowToList r rl
  , RowToList r' rl'
  , CoercibleRecordRL rl rl'
  ) => CoercibleRecord r r'

class CoercibleRecordRL (rl :: RowList) (rl' :: RowList)

instance coercibleRecordRLNil :: CoercibleRecordRL Nil Nil
else instance coercibleRecordRLConsDirect :: CoercibleRecordRL trl trl' => CoercibleRecordRL (Cons name typ trl) (Cons name typ trl')
else instance coercibleRecordRLConsOneOf ::
  ( CoercibleRecordRL trl trl'
  , InOneOf typ oneOfH oneOfT
  ) => CoercibleRecordRL (Cons name typ trl) (Cons name (OneOf oneOfH oneOfT) trl')
else instance coercibleRecordRLConsOptional ::
  ( CoercibleRecordRL trl trl'
  , HasUndefined oneOfH oneOfT
  ) => CoercibleRecordRL trl (Cons name (OneOf oneOfH oneOfT) trl')
