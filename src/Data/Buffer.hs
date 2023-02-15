{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Buffer where

import           Core.Include
import           Data.Class
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Module


data Buffer n t = Buffer
    { defBuffer  :: ModuleM ()
    , addrBuffer :: Ref Global (Array n (Stored t))
    }


buffer :: (IvoryZeroVal t, IvoryInit t, KnownNat n) => String -> Buffer n t
buffer id = Buffer { defBuffer  = defMemArea a
                   , addrBuffer = addrOf a
                   } where a    = area (id <> "_buffer") Nothing


instance Include (Buffer n t) where
    include = defBuffer


instance (KnownNat n, IvoryStore t) => Buff Buffer n t where
    getBuffer    = addrBuffer
