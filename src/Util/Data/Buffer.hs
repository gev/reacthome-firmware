{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Util.Data.Buffer where

import           GHC.TypeNats
import           Include
import           Ivory.Language
import           Ivory.Language.Module
import           Util.Data.Class


data Buffer n t = Buffer
  { defBuffer  :: ModuleM ()
  , addrBuffer :: Ref Global (Array n (Stored t))
  }


buffer :: (IvoryZeroVal t, IvoryInit t, KnownNat n) => String -> Buffer n t
buffer id = Buffer { defBuffer  = defMemArea a
                   , addrBuffer = addrOf a
                   }
             where a = area id Nothing



instance Include (Buffer n t) where
  include = defBuffer


instance (KnownNat n, IvoryStore t) => Buff Buffer n t where
  setItem b ix  = store (addrBuffer b ! ix)
  getItem b ix  = deref (addrBuffer b ! ix)
  process b f = f $ addrBuffer b
