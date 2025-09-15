{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.Touch where

import           Core.Handler
import           Ivory.Language

class Touch t where
    run         :: t -> Ivory (ProcEffects s ()) () 
    reset       :: t -> Ivory eff ()
    getDebug    :: t -> Ivory eff IFloat
    getState    :: t -> Ivory eff IBool
    isReady     :: t -> Ivory eff IBool
    start       :: t -> Ivory eff ()
    finish      :: t -> Ivory eff ()