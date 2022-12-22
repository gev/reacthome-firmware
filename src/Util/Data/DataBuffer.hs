{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}

module Util.Data.DataBuffer where

import           GHC.TypeNats
import           Include
import           Ivory.Language
import           Util.Data.Buffer


data DataBuffer n t =
  (KnownNat n, IvoryType t, IvoryStore t) =>
    DataBuffer (MemArea (Array n (Stored t)))


buffer :: (KnownNat n, IvoryType t, IvoryStore t, IvoryZeroVal t)
       => String -> DataBuffer n t
buffer id = DataBuffer $ area id Nothing



instance Include (DataBuffer n t) where
  include (DataBuffer b) = defMemArea b


-- instance Buffer DataBuffer n t where
storeItem (DataBuffer b) ix = store $ addrOf b ! ix
loadItem (DataBuffer b) ix = deref (addrOf b ! ix)
processBuffer (DataBuffer b) f = f $ addrOf b
