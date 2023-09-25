{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.DS18B20 where

import           Control.Monad            (replicateM_)
import           Control.Monad.Reader     (MonadReader, asks)
import           Control.Monad.State      (MonadState)
import           Core.Context
import           Core.Controller
import qualified Core.Domain              as D
import           Core.Feature
import           Core.Task
import qualified Core.Transport           as T
import           Data.Buffer
import           Data.Serialize
import           Data.Value
import           GHC.TypeNats
import           Interface.GPIO.OpenDrain (OpenDrain)
import           Interface.MCU            (peripherals)
import           Interface.OneWire
import           Ivory.Language
import           Ivory.Language.Uint      (Uint16 (Uint16))
import           Ivory.Stdlib
import           Prelude                  hiding (read)
import           Protocol.OneWire.Master



data DS18B20 = DS18B20
    { rxB      :: Values  9 Uint8
    , txB      :: Buffer 11 Uint8
    , count    :: Value     Uint8
    , transmit :: forall n. KnownNat n
               => Buffer n Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }


ds18b20 :: (MonadState Context m, MonadReader (D.Domain p t) m, T.Transport t, OpenDrain od)
        => (p -> m od -> m OneWire) -> (p -> m od) -> m Feature
ds18b20 ow od = do
    let name   = "ds18b20"
    mcu       <- asks $ peripherals . D.mcu
    transport <- asks D.transport
    rxB       <- buffer (name <> "_rx_buffer")
    txB       <- values (name <> "_tx_buffer") [ 0xc4
                                               , 0x28,1,2,3,4,5,6,7
                                               , 0, 0
                                               ]
    count     <- value  (name <> "_count"    ) 0
    let ds     = DS18B20 { rxB
                         , txB
                         , count
                         , transmit = T.transmitBuffer transport
                         }
    onewire   <- ow mcu $ od mcu
    master    <- mkOneWireMaster onewire (onData ds) (onError ds)


    addTask $ delay      10000     (name <> "_measure_temperature") $ measureTemperature master
    addTask $ delayPhase 10000 700 (name <> "_get_temperature"    ) $ getTemperature     master

    pure $ Feature ds



measureTemperature :: OneWireMaster -> Ivory eff ()
measureTemperature onewire = do
    reset    onewire
    skipROM  onewire
    write    onewire 0x44


getTemperature :: OneWireMaster -> Ivory eff ()
getTemperature onewire = do
    reset    onewire
    skipROM  onewire
    write    onewire 0xbe
    replicateM_ 9 $ read onewire




onData :: DS18B20 -> Uint8 -> Ivory (ProcEffects s ()) ()
onData DS18B20{..} v = do
    count' <- deref count
    store (rxB ! toIx count') v
    ifte_ (count' ==? 8)
          (do
            {-
                TODO: Check CRC
            -}
            store count 0
            raw <- unpackLE rxB 0
            let t = (25 * raw) `iDiv` 4 :: Sint16
            packLE txB 9 t
            transmit txB
          )
          (store count $ count' + 1)


onError :: DS18B20 -> Uint8 -> Ivory eff ()
onError _ _ = pure ()



instance Controller DS18B20
