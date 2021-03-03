module Untagged.Castable
       ( class Castable
       , class CastableRecordRL
       , cast
       ) where

import Foreign (Foreign)
import Literals.Undefined (Undefined)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Unsafe.Coerce (unsafeCoerce)

--| A `Castable a b` exists if all values of type `a` have
--| runtime values that can be interpreted as that of type `b`.
class Castable a b

instance castableIntNumber :: Castable Int Number
instance castableCharString :: Castable Char String
instance castableRecord ::
  ( RowToList r rl
  , RowToList r' rl'
  , CastableRecordRL rl rl'
  ) => Castable {|r} {|r'}
instance castableForeign :: Castable x Foreign

class CastableRecordRL (rl :: RowList Type) (rl' :: RowList Type)

instance castableRecordRLNil :: CastableRecordRL Nil Nil
else instance castableRecordRLConsDirect ::
  ( CastableRecordRL trl trl'
  ) => CastableRecordRL (Cons name typ trl) (Cons name typ trl')
else instance castableRecordRLConsCastable ::
  ( CastableRecordRL trl trl'
  , Castable typ typ'
  ) => CastableRecordRL (Cons name typ trl) (Cons name typ' trl')
else instance castableRecordRLConsOptional ::
  ( CastableRecordRL trl trl'
  , Castable Undefined t
  ) => CastableRecordRL trl (Cons name t trl')

cast :: forall a b. Castable a b => a -> b
cast = unsafeCoerce
