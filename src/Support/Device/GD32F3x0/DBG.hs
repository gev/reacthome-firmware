module Support.Device.GD32F3x0.DBG (
    getID,
    inclDBG,
) where

import Ivory.Language
import Ivory.Support.Device.GD32F3x0

getID :: Ivory eff Uint32
getID = call dbg_id_get

dbg_id_get :: Def ('[] :-> Uint32)
dbg_id_get = fun "dbg_id_get"

inclDBG :: ModuleDef
inclDBG = incl dbg_id_get
