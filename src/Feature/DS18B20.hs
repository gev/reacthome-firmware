{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}

module Feature.DS18B20
    ( DS18B20
    , ds18b20
    ) where

import           Control.Monad            (replicateM_, zipWithM_)
import           Control.Monad.Reader     (MonadReader, asks)
import           Control.Monad.State      (MonadState)
import           Core.Context
import           Core.Controller
import qualified Core.Domain              as D
import           Core.Feature
import           Core.Task
import qualified Core.Transport           as T
import           Data.Buffer
import           Data.Matrix
import           Data.Serialize
import           Data.Value
import           GHC.TypeNats
import           Interface.Flash          (Flash (address))
import           Interface.GPIO.OpenDrain (OpenDrain)
import           Interface.MCU            (peripherals)
import           Interface.OneWire
import           Ivory.Language
import           Ivory.Language.Uint      (Uint16 (Uint16))
import           Ivory.Stdlib
import           Prelude                  hiding (read)
import           Protocol.OneWire.Master



data DS18B20 = DS18B20
    { rxB       :: Values     9 Uint8
    , txB       :: Buffer    11 Uint8
    , dsErrB    :: Buffer     9 Uint8
    , owErrB    :: Buffer     2 Uint8
    , idNumber  :: Value        Uint8
    , idList    :: Matrix 256 8 Uint8
    , transmit  :: forall n. KnownNat n
                => Buffer n Uint8 -> forall s. Ivory (ProcEffects s ()) ()
    }


ds18b20 :: (MonadState Context m, MonadReader (D.Domain p t) m, T.Transport t, OpenDrain od)
        => (p -> m od -> m OneWire) -> (p -> m od) -> m Feature
ds18b20 ow od = do
    let name   = "ds18b20"
    mcu       <- asks $ peripherals . D.mcu
    transport <- asks D.transport
    rxB       <- buffer  (name <> "_rx_buffer"      )
    txB       <- values  (name <> "_tx_buffer"      ) [0xc6, 0,0,0,0,0,0,0,0, 0,0]
    dsErrB    <- values  (name <> "_ds_error_buffer") [0xc6, 0,0,0,0,0,0,0,0]
    owErrB    <- values  (name <> "_ow_error_buffer") [0xc6, 0]
    idNumber  <- value   (name <> "_id_number"      ) 0
    idList    <- matrix_ (name <> "_id_list"        )

    onewire   <- ow mcu $ od mcu

    let ds     = DS18B20 { rxB
                         , txB
                         , dsErrB
                         , owErrB
                         , idNumber
                         , idList
                         , transmit = T.transmitBuffer transport
                         }

    master    <- mkOneWireMaster onewire (onData ds) (onDiscovery ds) (onError ds)

    addProc getCRC

    addTask $ delay      15_000       (name <> "_search"             ) $ searchDevices      ds master
    addTask $ delayPhase 15_000 6_000 (name <> "_measure_temperature") $ measureTemperature ds master
    addTask $ delayPhase 15_000 6_700 (name <> "_get_temperature"    ) $ getTemperature     ds master

    pure $ Feature ds



searchDevices :: DS18B20 -> OneWireMaster -> Ivory eff ()
searchDevices DS18B20{..} onewire = do
    store  idNumber 0
    search onewire 256



measureTemperature :: DS18B20 -> OneWireMaster -> Ivory eff ()
measureTemperature DS18B20{..} onewire = do
    idNumber' <- deref idNumber
    when (idNumber' >? 0) $ do
        reset    onewire
        skipROM  onewire
        write    onewire 0x44


getTemperature :: DS18B20 -> OneWireMaster -> Ivory eff ()
getTemperature DS18B20{..} onewire = do
    idNumber' <- deref idNumber
    for (toIx idNumber') $ \ix -> do
        reset    onewire
        matchROM onewire
        let id = idList ! ix
        arrayMap $ \ix -> write onewire =<< deref (id ! ix)
        write onewire 0xbe
        for (9 :: Ix 10) $
            read onewire (cast ix) . cast

{-
    TODO: Move cast to a separate utility module
-}
cast :: (KnownNat n, SafeCast t Sint32, Default t, Bounded t, IvoryOrd t) => Ix n -> t
cast = castDefault . fromIx



onData :: DS18B20 -> Uint8 -> Uint8 -> Uint8 -> Ivory (ProcEffects s ()) ()
onData DS18B20{..} i index v = do
    store (rxB ! toIx index) v
    when (index ==? 8) $ do
        crc  <- call getCRC rxB
        let id = idList ! toIx i
        ifte_ (crc ==? 0)
              (do
                    arrayCopy txB id 1 8
                    raw <- unpackLE rxB 0
                    let t = (25 * raw) `iDiv` 4 :: Sint16
                    packLE txB 9 t
                    transmit txB

              )
              (do
                    arrayCopy dsErrB id 1 8
                    transmit dsErrB
              )



onDiscovery :: DS18B20 -> Uint8 -> Buffer 8 Uint8 -> Ivory (ProcEffects s ()) ()
onDiscovery DS18B20{..} _ id = do
    t <- deref (id ! 0)
    when (t ==? 0x28) $ do
        idNumber' <- deref idNumber
        let id'  = idList ! toIx idNumber'
        arrayMap $ \ix -> store (id' ! ix) =<< deref (id ! ix)
        store idNumber $ idNumber' + 1



onError :: DS18B20 -> Uint8 -> Ivory (ProcEffects s ()) ()
onError DS18B20{..} error = do
    store (owErrB ! 1) error
    transmit owErrB
    when (error ==? errorNoPresence .|| error ==? errorNotReady) $
        store idNumber 0


getCRC :: Def ('[Buffer 9 Uint8] :-> Uint8)
getCRC = proc "ds18b20_get_crc" $ \buff -> body $ do
    crc <- local $ ival 0
    arrayMap $ \ix -> do
        inbyte <- local . ival =<< deref (buff ! ix)
        times (8 :: Ix 9) . const $ do
            crc' <- deref crc
            inbyte' <- deref inbyte
            let mix = (crc' .^ inbyte') .& 0x01
            store crc $ crc' `iShiftR` 1
            when (mix /=? 0) $ do
                crc'' <- deref crc
                store crc $ crc'' .^ 0x8c
            store inbyte $ inbyte' `iShiftR` 1
    ret =<< deref crc



instance Controller DS18B20
