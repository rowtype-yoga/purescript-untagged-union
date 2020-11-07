module Untagged.Coercible
       ( class Coercible
       , class CoercibleRecordRL
       , coerce
       ) where

import Data.Array.NonEmpty as NA
import Data.Either (Either)
import Data.List as L
import Data.List.Lazy as LL
import Data.List.Lazy.NonEmpty as LNL
import Data.List.NonEmpty as NL
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Foreign (Foreign)
import Literals.Undefined (Undefined)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)

--| A `Coercible a b` exists if all values of type `a` have
--| runtime values that can be interpreted as that of type `b`.
class Coercible a b

instance coercibleIntNumber :: Coercible Int Number
instance coercibleCharString :: Coercible Char String
instance coercibleArray :: Coercible a b => Coercible (Array a) (Array b)
instance coercibleMaybe :: Coercible a b => Coercible (Maybe a) (Maybe b)
instance coercibleTuple ::
  (Coercible a b, Coercible c d) => Coercible (Tuple a c) (Tuple b d)
instance coercibleNonEmptyArray ::
  Coercible a b => Coercible (NA.NonEmptyArray a) (NA.NonEmptyArray b)
instance coercibleList :: Coercible a b => Coercible (L.List a) (L.List b)
instance coercibleLazyList :: Coercible a b => Coercible (LL.List a) (LL.List b)
instance coercibleNonEmptyList ::
  Coercible a b => Coercible (NL.NonEmptyList a) (NL.NonEmptyList b)
instance coercibleLazyNonEmptyList ::
  Coercible a b => Coercible (LNL.NonEmptyList a) (LNL.NonEmptyList b)
instance coercibleMap :: Coercible a b => Coercible (Map k a) (Map k b)
instance coercibleEither ::
  (Coercible a b, Coercible c d) => Coercible (Either a c) (Either b d)
instance coercibleRecord ::
  ( RowToList r rl
  , RowToList r' rl'
  , CoercibleRecordRL rl rl'
  ) => Coercible {|r} {|r'}
instance coercibleForeign :: Coercible x Foreign

class CoercibleRecordRL (rl :: RowList) (rl' :: RowList)

instance coercibleRecordRLNil :: CoercibleRecordRL Nil Nil
else instance coercibleRecordRLConsDirect ::
  ( CoercibleRecordRL trl trl'
  ) => CoercibleRecordRL (Cons name typ trl) (Cons name typ trl')
else instance coercibleRecordRLConsCoercible ::
  ( CoercibleRecordRL trl trl'
  , Coercible typ typ'
  ) => CoercibleRecordRL (Cons name typ trl) (Cons name typ' trl')
else instance coercibleRecordRLConsOptional ::
  ( CoercibleRecordRL trl trl'
  , Coercible Undefined t
  ) => CoercibleRecordRL trl (Cons name t trl')

coerce :: forall a b. Coercible a b => a -> b
coerce = unsafeCoerce
