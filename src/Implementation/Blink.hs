{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}

module Implementation.Blink where

import           Control.Monad.Reader
import           Control.Monad.State
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Task
import           Data.Value
import           Interface.GPIO.Output
import           Interface.GPIO.Port
import           Interface.MCU         (MCU (peripherals))
import           Interface.Timer
import           Ivory.Language

blink :: (MonadState Context m, MonadReader (Domain p ()) m, Output o, Pull p u)
      => (p -> u -> m o) -> m ()
blink o = do
    let name  = "blink"

    mcu      <- asks mcu
    let peripherals' = peripherals mcu
    out      <- o peripherals' $ pullNone peripherals'
    state    <- value (name <> "_state") false

    addTask $ delay 1_000 name $ do
        v <- deref state
        store state $ iNot v
        ifte_ v (set   out)
                (reset out)
