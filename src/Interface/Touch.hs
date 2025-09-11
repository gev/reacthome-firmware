{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.Touch where

import           Core.Handler
import           Ivory.Language

class Touch t where
    run       :: t -> Ivory (ProcEffects s ()) () -> Ivory (ProcEffects s ()) ()
    reset     :: t -> Ivory eff ()
    getTime   :: t -> Ivory eff IFloat
    getState  :: t -> Ivory eff IBool
