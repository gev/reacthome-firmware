{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.DBG
  ( getID
  , inclDBG
  ) where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Support
import           Ivory.Support.Device.GD32F3x0


inclDBG :: ModuleM ()
inclDBG = do
  incl dbg_id_get


getID :: Ivory eff Uint32
getID = call dbg_id_get

dbg_id_get :: Def ('[] :-> Uint32)
dbg_id_get = fun "dbg_id_get"
