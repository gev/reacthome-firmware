{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}

module Feature.Lanamp where

import           Control.Monad.Reader  (MonadReader, asks)
import           Control.Monad.State   (MonadState)
import           Core.Context
import           Core.Domain           as D
import           Core.Handler
import           Core.Task
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Device.GD32F4xx.I2SRX
import           Device.GD32F4xx.I2STX
import           GHC.TypeLits
import           Interface.I2SRX
import           Interface.I2STX
import           Interface.MCU
import           Ivory.Language




data Lanamp t r = Lanamp
    { i2stx    :: I2STX  t
    , i2sBuff  :: Buffer 2000 Uint32
    , i2sQueue :: Queue  2000
    , i2srx    :: I2SRX  r
    , i2sWord  :: Value Uint32
    }

mkLanAmp :: ( MonadState Context m
            , MonadReader (D.Domain p c) m
            , KnownNat t, KnownNat r)
            => (p -> m (I2STX t)) -> (p -> m (I2SRX r))
            -> m (Lanamp t r)
mkLanAmp i2stx' i2srx' = do
    let  name   =   "lanamp"
    mcu         <-  asks D.mcu
    i2sBuff     <-  values' (name <> "_i2sBuff") 0
    i2sQueue    <-  queue   (name <> "_i2sQueue")
    i2stx       <-  i2stx' $ peripherals mcu
    i2srx       <-  i2srx' $ peripherals mcu
    i2sWord     <-  value (name <> "_word1") 0

    let lanamp = Lanamp { i2stx
                        , i2sBuff
                        , i2sQueue
                        , i2srx
                        , i2sWord
                        }


    addHandler $ HandleI2SRX i2srx (receive lanamp)
    addHandler $ HandleI2STX i2stx (transmit lanamp)

    pure lanamp



receive :: Lanamp t r -> Uint32 -> Ivory eff ()
receive (Lanamp {..}) word =
    push i2sQueue $ \i -> do
        store (i2sBuff ! toIx i) word


transmit :: Lanamp t r -> Ivory eff Uint32
transmit (Lanamp {..}) = do
    flip (pop' i2sQueue) (store i2sWord 0) $ \i -> do
        store i2sWord =<< deref (i2sBuff ! toIx i)
    deref i2sWord
