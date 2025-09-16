{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.Touch where

import           Core.Handler
import           Ivory.Language

class Touch t where
    run         :: t -> Ivory (ProcEffects s ()) ()
    getDebug    :: t -> Ivory eff IFloat
    getState    :: t -> Ivory eff IBool
    reset       :: t -> Ivory eff ()
    pause       :: t -> Ivory (ProcEffects s ()) ()
