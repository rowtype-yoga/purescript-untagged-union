module Untagged.Coercible
       ( class Coercible
       , class CoercibleRecordRL
       , coerce
       ) where

import Data.Array.NonEmpty as NA
import Foreign (Foreign)
import Foreign.Object (Object)
import Literals.Undefined (Undefined)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)

--| A `Coercible a b` exists if all values of type `a` have
--| runtime values that can be interpreted as that of type `b`.
class Coercible a b

instance coercibleIntNumber :: Coercible Int Number
instance coercibleCharString :: Coercible Char String
instance coercibleArray :: Coercible a b => Coercible (Array a) (Array b)
instance coercibleNonEmptyArray ::
  Coercible a b => Coercible (NA.NonEmptyArray a) (NA.NonEmptyArray b)
instance coercibleObject ::
  Coercible a b => Coercible (Object a) (Object b)
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
