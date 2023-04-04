{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F4xx.DBG
    ( getID
    , inclDBG
    ) where

import           Ivory.Language
import           Ivory.Support.Device.GD32F4xx



getID :: Ivory eff Uint32
getID = call dbg_id_get

dbg_id_get :: Def ('[] :-> Uint32)
dbg_id_get = fun "dbg_id_get"



inclDBG :: ModuleDef
inclDBG = incl dbg_id_get
