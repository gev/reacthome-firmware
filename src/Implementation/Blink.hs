module Implementation.Blink where

import Control.Monad.Reader
import Control.Monad.State
import Core.Context
import Core.Domain
import Core.Domain qualified as D
import Core.Meta
import Core.Task
import Data.Value
import Interface.GPIO.Output
import Interface.GPIO.Port
import Interface.MCU
import Interface.MCU qualified as I
import Ivory.Language

blink ::
    ( MonadState Context m
    , MonadReader (Domain p ()) m
    , Output o
    , Pull p u
    ) =>
    (p -> u -> m o) ->
    m ()
blink out = do
    let name = "blink"
    meta <- asks D.meta
    platform <- I.platform meta.mcu
    out' <- out platform.peripherals $ pullNone platform.peripherals
    state <- value (name <> "_state") false
    addTask $ delay 1 name do
        v <- deref state
        store state $ iNot v
        ifte_
            v
            (set out')
            (reset out')
