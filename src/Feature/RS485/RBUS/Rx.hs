module Feature.RS485.RBUS.Rx where

import           Feature.RS485.RBUS.Data
import           Ivory.Language


rxHandle :: RBUS -> Uint16 -> Ivory eff ()
rxHandle _ _ = pure ()
