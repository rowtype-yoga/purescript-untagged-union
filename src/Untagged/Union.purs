module Untagged.Union
       ( OneOf
       , type (|+|)
       , class InOneOf
       , UndefinedOr
       , asOneOf
       , fromOneOf
       , toEither1
       , getLeft
       , getLeft'
       , getRight
       , getRight'
       , uorToMaybe
       , maybeToUor
       , withUor
       , fromUndefinedOr
       , class Reducible
       , reduce
       ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Foreign (unsafeToForeign)
import Literals.Undefined (Undefined, undefined)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Coercible (class Coercible, coerce)
import Untagged.TypeCheck (class HasRuntimeType, hasRuntimeType)

foreign import data OneOf :: Type -> Type -> Type

instance oneOfEq :: (Eq a, Eq b, HasRuntimeType a) => Eq (OneOf a b) where
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

type UndefinedOr a = OneOf Undefined a

asOneOf :: forall a h t. Coercible a (OneOf h t) => a -> OneOf h t
asOneOf = coerce

fromOneOf :: forall h t a. InOneOf a h t => HasRuntimeType a => OneOf h t -> Maybe a
fromOneOf f =
  if hasRuntimeType (Proxy :: Proxy a) (unsafeToForeign f)
  then Just $ unsafeCoerce f
  else Nothing

withOneOf :: forall a b x. HasRuntimeType a => (a -> x) -> (b -> x) -> OneOf a b -> x
withOneOf f g o =
  if isTypeA (unsafeToForeign o)
  then f (unsafeCoerce o)
  else g (unsafeCoerce o)
  where
    isTypeA = hasRuntimeType (Proxy :: Proxy a)

--| Unwraps a single layer of `OneOf` to an Either
--| Note that for some `x :: a |+| b`. If the value `x` has a runtime
--| value that can be read as either types `a` and `b`, then
--| `toEither1 x` will return `Left`.
--|
--| Example: toEither1 (asOneOf 3.0 :: Int |+| Number) == Left 3
toEither1 :: forall a b. HasRuntimeType a => OneOf a b -> Either a b
toEither1 = withOneOf Left Right

getLeft :: forall a b. HasRuntimeType a => OneOf a b -> Maybe a
getLeft o =
  if isTypeA (unsafeToForeign o)
  then Just (unsafeCoerce o)
  else Nothing
  where
    isTypeA = hasRuntimeType (Proxy :: Proxy a)

getLeft' :: forall a b. HasRuntimeType b => OneOf a b -> Maybe a
getLeft' o =
  if isTypeB (unsafeToForeign o)
  then Nothing
  else Just (unsafeCoerce o)
  where
    isTypeB = hasRuntimeType (Proxy :: Proxy b)

getRight :: forall a b. HasRuntimeType b => OneOf a b -> Maybe b
getRight o =
  if isTypeB (unsafeToForeign o)
  then Just (unsafeCoerce o)
  else Nothing
  where
    isTypeB = hasRuntimeType (Proxy :: Proxy b)

getRight' :: forall a b. HasRuntimeType a => OneOf a b -> Maybe b
getRight' o =
  if isTypeA (unsafeToForeign o)
  then Nothing
  else Just (unsafeCoerce o)
  where
    isTypeA = hasRuntimeType (Proxy :: Proxy a)

uorToMaybe :: forall a. UndefinedOr a -> Maybe a
uorToMaybe = getRight'

maybeToUor :: forall a. Maybe a -> UndefinedOr a
maybeToUor (Just a) = unsafeCoerce a
maybeToUor Nothing = coerce undefined

withUor :: forall a b. (a -> b) -> UndefinedOr a -> UndefinedOr b
withUor f = withUor' (unsafeCoerce <<< f)

withUor' :: forall a b. (a -> UndefinedOr b) -> UndefinedOr a -> UndefinedOr b
withUor' f o = withOneOf (const (coerce undefined :: UndefinedOr b)) f o

fromUndefinedOr :: forall a. a -> UndefinedOr a -> a
fromUndefinedOr a = fromMaybe a <<< uorToMaybe

class Reducible f i o | i -> f o, f o -> i where
  reduce :: f -> i -> o

instance reduceOneOf ::
  ( Reducible tf b o
  , HasRuntimeType a
  ) => Reducible ((a -> o) /\ tf) (OneOf a b) o where
  reduce (f /\ tf) o =
    case toEither1 o of
      Left a -> f a
      Right b -> reduce tf b
else instance reduceA :: Reducible (a -> b) a b where
  reduce = ($)
