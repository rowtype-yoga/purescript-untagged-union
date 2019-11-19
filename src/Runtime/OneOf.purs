module Runtime.OneOf
       ( OneOf
       , type (|+|)
       , class InOneOf
       , UndefinedOr
       , asOneOf
       , fromOneOf
       , toEither1
       , class Reducible
       , reduce
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Foreign (unsafeToForeign)
import Runtime.Coercible (class Coercible, coerce)
import Runtime.TypeCheck (class HasRuntimeType, hasRuntimeType)
import Runtime.Undefined (Undefined)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

foreign import data OneOf :: Type -> Type -> Type

instance oneOfEq :: (Eq a, Eq b, HasRuntimeType a, HasRuntimeType b) => Eq (OneOf a b) where
  eq o o' = case toEither1 o, toEither1 o' of
    Left a, Left a' -> a == a'
    Right b, Right b' -> b == b'
    _, _ -> false

instance hasRuntimeTypeOneOf :: (HasRuntimeType a, HasRuntimeType a') => HasRuntimeType (OneOf a a') where
  hasRuntimeType _ x = hasRuntimeType (Proxy :: Proxy a) x || hasRuntimeType (Proxy :: Proxy a') x

infixr 7 type OneOf as |+|

class InOneOf a h t
instance inOneOfHead :: InOneOf a a t
else instance inOneOfLast :: InOneOf a h a
else instance inOneOfTail :: (InOneOf a h' t') => InOneOf a h (OneOf h' t')

instance coercibleOneOf :: InOneOf a h t => Coercible a (OneOf h t)

type UndefinedOr a = OneOf a Undefined

asOneOf :: forall a h t. Coercible a (OneOf h t) => a -> OneOf h t
asOneOf = coerce

fromOneOf :: forall h t a. InOneOf a h t => HasRuntimeType a => OneOf h t -> Maybe a
fromOneOf f =
  if hasRuntimeType (Proxy :: Proxy a) (unsafeToForeign f)
  then Just $ unsafeCoerce f
  else Nothing

--| Unwraps a single layer of `OneOf` to an Either
--| Note that for some `x :: a |+| b`. If the value `x` has a runtime
--| value that can be read as either types `a` and `b`, then
--| `toEither1 x` will return `Left`.
--|
--| Example: toEither1 (asOneOf 3.0 :: Int |+| Number) == Left 3
toEither1 :: forall a b. HasRuntimeType a => HasRuntimeType b => OneOf a b -> Either a b
toEither1 o =
  if isTypeA (unsafeToForeign o)
  then Left (unsafeCoerce o)
  else Right (unsafeCoerce o)
  where
    isTypeA = hasRuntimeType (Proxy :: Proxy a)

class Reducible f i o | i -> f o, f o -> i where
  reduce :: f -> i -> o

instance reduceOneOf ::
  ( Reducible tf b o
  , HasRuntimeType a
  , HasRuntimeType b
  ) => Reducible ((a -> o) /\ tf) (OneOf a b) o where
  reduce (f /\ tf) o =
    case toEither1 o of
      Left a -> f a
      Right b -> reduce tf b
else instance reduceA :: Reducible (a -> b) a b where
  reduce = ($)
