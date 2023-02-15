{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}

module Interface.Mac (Mac, Mac', mac) where

import           GHC.TypeNats
import           Include
import           Initialize
import           Ivory.Language
import           Ivory.Language.Module
import           Util.Data.Buffer
import           Util.Data.Class


type Mac = Buffer 6 Uint8


data Mac' = Mac
    { name          :: String
    , includeMac    :: ModuleM ()
    , initializeMac :: Buffer 6 Uint8 -> forall eff. Ivory eff ()
    , getMac        :: Buffer 6 Uint8
    }


mac :: ModuleM ()
    -> (Mac -> forall eff. Ivory eff ())
    -> String
    -> Mac'
mac include initialize name = Mac
    { name          = name
    , includeMac    = include
    , initializeMac = initialize
    , getMac        = buffer name
    }


instance Include Mac' where
    include (Mac {includeMac, getMac}) =
        includeMac >> include getMac


instance Initialize Mac' where
    initialize (Mac {name, initializeMac, getMac}) =
        [proc (name <> "_init") $ body $ initializeMac getMac]
