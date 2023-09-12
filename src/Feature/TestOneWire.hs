{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

module Feature.TestOneWire where

import           Control.Monad.Reader
import           Control.Monad.State      (MonadState)
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Task
import           Data.Value
import qualified Interface.GPIO.OpenDrain as I
import           Interface.MCU            (MCU (peripherals))
import qualified Interface.OneWire        as I
import           Interface.Timer
import           Ivory.Language

data TestOW where
    TestOW :: I.OneWire ow
          => { name     :: String
             , onewire  :: ow
             } -> TestOW


testOneWire :: (MonadState Context m, MonadReader (Domain p t) m, I.OneWire ow, I.OpenDrain od)
            => (p -> m od -> m ow) -> (p -> m od) -> m Feature
testOneWire ow od = do
    let name  = "test_one_wire"
    mcu      <- asks mcu
    onewire  <- (ow $ peripherals mcu) (od $ peripherals mcu)

    let feature = Feature $ TestOW { name, onewire }

    addTask $ delay 1_000 name $ do
        I.write onewire 0xaa

    pure feature

instance Controller TestOW
