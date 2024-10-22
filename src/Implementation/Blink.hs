{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}

module Implementation.Blink where

import           Control.Monad.Reader
import           Control.Monad.State
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Handler          (Handler (addHandler))
import           Core.Task
import           Data.Value
import           Interface.GPIO.Output
import           Interface.GPIO.Port
import           Interface.MCU         (MCU (peripherals))
import           Interface.Timer       (HandleTimer (..), Timer)
import           Ivory.Language

blink :: (MonadState Context m, MonadReader (Domain p ()) m, Output o, Pull p u, Timer t)
      => (p -> u -> m o) -> (p -> Uint32 -> Uint32 -> m t) -> m ()
blink out timer = do
    let name          = "blink"
    mcu              <- asks mcu
    let peripherals'  = peripherals mcu
    out'             <- out peripherals' $ pullNone peripherals'
    timer'           <- timer peripherals' 1_000 1
    state            <- value (name <> "_state") false
    addHandler $ HandleTimer timer' $ do
            v <- deref state
            store state $ iNot v
            ifte_ v (set   out')
                    (reset out')
