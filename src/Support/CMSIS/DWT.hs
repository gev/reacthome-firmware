{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.CMSIS.DWT
  ( dwtDelayInit
  , dwtDelay
  , inclDWT
  ) where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Language.Proc
import           Ivory.Language.Syntax

fun :: ProcType t => Sym -> Def t
fun = importProc "core_cm4.h"

inclDWT :: ModuleM ()
inclDWT = do
  incl dwt_delay_init
  incl dwt_delay


dwtDelayInit :: Ivory eff ()
dwtDelayInit = call_ dwt_delay_init

dwt_delay_init :: Def ('[] :-> ())
dwt_delay_init = fun  "dwt_delay_init"


dwtDelay :: Uint32 -> Ivory eff ()
dwtDelay = call_ dwt_delay

dwt_delay :: Def ('[Uint32] :-> ())
dwt_delay = fun  "dwt_delay_init"
