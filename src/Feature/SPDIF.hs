
module Feature.SPDIF where

import Control.Monad.State
import Core.Context
import Core.Handler
import Data.Queue
import Data.Record
import GHC.TypeNats
import Interface.I2S
import Interface.I2SRX
import Ivory.Language

data SPDIF n = SPDIF
    { i2sSpdifQueue :: Queue n (Records n SampleStruct)
    , i2sSpdifSample :: Sample
    }

mkSpdif ::
    ( MonadState Context m
    , KnownNat n
    , Handler HandleI2SRX i
    ) =>
    i ->
    m (SPDIF n)
mkSpdif i2s = do
    let name = "spdif"
    i2sSpdifQueue <-
        queue (name <> "_i2s_spdif_queue")
            =<< records_ (name <> "_i2s_buff_spdif")
    i2sSpdifSample <-
        record
            (name <> "_i2s_spdif_sample")
            [left .= izero, right .= izero]

    let spdif = SPDIF{i2sSpdifQueue, i2sSpdifSample}

    addHandler $ HandleI2SRX i2s (receiveI2S spdif)

    return spdif

receiveI2S :: (KnownNat n) => SPDIF n -> Sample -> Ivory eff ()
receiveI2S SPDIF{..} sample =
    push i2sSpdifQueue $ \i2sSpdifBuff i -> do
        l <- deref (sample ~> left)
        r <- deref (sample ~> right)
        store (i2sSpdifBuff ! toIx i ~> left) $ l `iDiv` 256
        store (i2sSpdifBuff ! toIx i ~> right) $ r `iDiv` 256

getSpdifSample :: (KnownNat n) => SPDIF n -> Ivory eff Sample
getSpdifSample SPDIF{..} = do
    pop i2sSpdifQueue $ \buff i -> do
        i2sSpdifSample <== buff ! toIx i
    pure i2sSpdifSample
