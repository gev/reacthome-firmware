{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Interface.Mac (Mac, mac, getMac) where

import           Core.Include
import           Core.Initialize
import           Data.Buffer
import           GHC.TypeNats
import           Ivory.Language
import           Ivory.Language.Module


data Mac = Mac
    { name          :: String
    , initializeMac :: Buffer 6 Uint8 -> forall eff. Ivory eff ()
    , getMac        :: Buffer 6 Uint8
    }


mac :: (Buffer 6 Uint8 -> forall eff. Ivory eff ())
    -> String
    -> Mac
mac initialize name = Mac
    { name          = name
    , initializeMac = initialize
    , getMac        = buffer name
    }


instance Include Mac where
    include (Mac {..}) = include getMac


instance Initialize Mac where
    initialize (Mac {..}) =
        [proc (name <> "_init") $ body $ initializeMac getMac]
