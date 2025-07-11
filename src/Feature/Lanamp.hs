{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Lanamp where

import           Control.Monad.Reader   (MonadReader, asks)
import           Control.Monad.State    (MonadState)
import           Core.Context
import           Core.Domain            as D
import           Core.Handler
import           Core.Task
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Queue
import           Data.Record
import           Data.Value
import           Device.GD32F4xx.I2STRX
import           GHC.TypeLits
import           Interface.GPIO.Output
import           Interface.GPIO.Port
import           Interface.I2S
import           Interface.I2SRX
import           Interface.I2STX
import           Interface.MCU
import           Ivory.Language




data Lanamp t r = Lanamp
    { i2sTrx   :: I2STRX  t r
    , i2sQueue :: Queue 256 (Records 256 SampleStruct)
    , i2sWord  :: Sample
    }

mkLanAmp :: ( MonadState Context m
            , MonadReader (D.Domain p c) m
            , KnownNat t, KnownNat r, Pull p u, Output o)
            => (p -> m (I2STRX t r))
            -> (p -> u -> m o)
            -> m (Lanamp t r)
mkLanAmp i2sTrx' shutdown' = do
    let  name   =   "lanamp"
    mcu         <-  asks D.mcu
    let peripherals' = peripherals mcu
    i2sQueue    <-  queue (name <> "_i2sQueue") =<< records' (name <> "_i2sBuff") [left .= izero, right .= izero]
    i2sTrx      <-  i2sTrx' $ peripherals mcu
    shutdown    <-  shutdown' peripherals' $ pullNone peripherals'
    i2sWord     <-  record (name <> "_word1") [left .= izero, right .= izero]

    let lanamp = Lanamp { i2sTrx
                        , i2sQueue
                        , i2sWord
                        }

    addInit "lanamp_init" $ set shutdown

    addHandler $ HandleI2SRX i2sTrx (receive lanamp)
    addHandler $ HandleI2STX i2sTrx (transmit lanamp)

    pure lanamp



receive :: Lanamp t r -> Sample -> Ivory eff ()
receive (Lanamp {..}) word =
    push i2sQueue $ \i2sBuff ix -> do
        store (i2sBuff ! ix ~> left) =<< deref (word ~> left)
        store (i2sBuff ! ix ~> right) =<< deref (word ~> right)


transmit :: Lanamp t r -> Ivory eff Sample
transmit (Lanamp {..}) = do
    pop i2sQueue $ \i2sBuff ix -> do
        store (i2sWord ~> left) =<< deref (i2sBuff ! ix ~> left)
        store (i2sWord ~> right)=<< deref (i2sBuff ! ix ~> right)
    pure i2sWord
