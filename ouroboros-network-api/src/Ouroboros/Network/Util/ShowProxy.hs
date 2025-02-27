{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds         #-}

module Ouroboros.Network.Util.ShowProxy
  ( ShowProxy (..)
  , Proxy (..)
  ) where

import           Data.Typeable

class ShowProxy p where
    showProxy :: Proxy p -> String

    default showProxy :: Typeable p => Proxy p -> String
    showProxy p = showsTypeRep (typeRep p) ""

instance ShowProxy Int where
