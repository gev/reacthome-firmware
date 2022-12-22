{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Util.Data.MessageBuffer where

import           GHC.TypeNats
import           Include
import           Ivory.Language
import           Util.Data.Buffer


data MessageBuffer n t =
  (KnownNat n, IvoryType t, IvoryStore t) =>
    MessageBuffer (MemArea (Array n (Stored t)))


messageBuffer :: (KnownNat n, IvoryType t, IvoryStore t, IvoryZeroVal t)
       => String -> MessageBuffer n t
messageBuffer id = MessageBuffer $ area (id <> "_buffer") Nothing


instance Include (MessageBuffer n t) where
  include (MessageBuffer b) = defMemArea b


instance Buffer MessageBuffer n t where
  storeItem (MessageBuffer b) ix = store $ addrOf b ! ix
  loadItem (MessageBuffer b) ix = deref (addrOf b ! ix)
  processBuffer (MessageBuffer b) f = f $ addrOf b
