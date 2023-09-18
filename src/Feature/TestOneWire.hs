{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Feature.TestOneWire where

import           Control.Monad.Reader
import           Control.Monad.State      (MonadState)
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Task
import           Data.Value
import qualified Interface.GPIO.OpenDrain as OD
import           Interface.MCU            (MCU (peripherals))
import qualified Interface.OneWire        as OW
import qualified Interface.RS485          as OW
import           Interface.Timer
import           Ivory.Language

data TestOW where
    TestOW :: OW.OneWire ow
          => { name     :: String
             , onewire  :: ow
             } -> TestOW


testOneWire :: (MonadState Context m, MonadReader (Domain p t) m, OW.OneWire ow, OD.OpenDrain od)
            => (p -> m od -> m ow) -> (p -> m od) -> m Feature
testOneWire ow od = do
    let name  = "test_one_wire"
    mcu      <- asks mcu
    onewire  <- (ow $ peripherals mcu) (od $ peripherals mcu)

    let feature = Feature $ TestOW { name, onewire }

    addTask $ delay 20 name $ do
        OW.reset onewire
        mapM_ (OW.write onewire) [0xcc, 0xbe]
        replicateM_ 9 $ OW.read onewire

    pure feature

instance Controller TestOW
