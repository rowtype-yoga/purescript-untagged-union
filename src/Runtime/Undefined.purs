module Runtime.Undefined
       ( Undefined
       , undefined
       ) where

import Prelude

foreign import data Undefined :: Type

instance undefinedEq :: Eq Undefined where
  eq _ _ = true

foreign import undefined :: Undefined
