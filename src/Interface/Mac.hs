{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}

module Interface.Mac (Mac, mac) where

import           GHC.TypeNats
import           Include
import           Initialize
import           Ivory.Language
import           Ivory.Language.Module
import           Util.Data.Buffer
import           Util.Data.Class


type Mac = Mac' 6 Uint8


data Mac' n t = Mac
    { name          :: String
    , includeMac    :: ModuleM ()
    , initializeMac :: Buffer n t -> forall eff. Ivory eff ()
    , buff          :: Buffer n t
    }


mac :: ModuleM ()
    -> (Buffer 6 Uint8 -> forall eff. Ivory eff ())
    -> String
    -> Mac
mac include initialize name = Mac
    { name          = name
    , includeMac    = include
    , initializeMac = initialize
    , buff          = buffer name
    }


instance Include (Mac' n t) where
    include (Mac {includeMac, buff}) =
        includeMac >> include buff


instance Initialize (Mac' n t) where
    initialize (Mac {name, initializeMac, buff}) =
        [proc (name <> "_init") $ body $ initializeMac buff]


instance (IvoryStore t, KnownNat n) => Buff Mac' n t where
  getBuffer = getBuffer . buff
