{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.CMSIS.DWT
  ( dwtCycleCounter
  , dwtDelayInit
  , dwtDelay
  , inclDWT
  ) where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Language.Proc
import           Ivory.Language.Syntax

h = "dwt.h"

inclDWT :: ModuleM ()
inclDWT = do
  inclSym dwtCycleCounter
  incl dwt_delay_init
  incl dwt_delay


dwtCycleCounter :: Uint32
dwtCycleCounter = extern "dwt_cycle_counter" h

dwtDelayInit :: Ivory eff ()
dwtDelayInit = call_ dwt_delay_init

dwt_delay_init :: Def ('[] :-> ())
dwt_delay_init = importProc "dwt_delay_init" h


dwtDelay :: Uint32 -> Ivory eff ()
dwtDelay = call_ dwt_delay

dwt_delay :: Def ('[Uint32] :-> ())
dwt_delay = importProc "dwt_delay" h
