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
import           Support.Ivory

(_, fun) = include "gd32f3x0_dbg.h"

inclDBG :: ModuleM ()
inclDBG = do
  incl dbg_id_get


getID :: Ivory eff Uint32
getID = 
  call dbg_id_get

dbg_id_get :: Def ('[] :-> Uint32)
dbg_id_get = fun "dbg_id_get"