module Feature.I2SReceive where

import Control.Monad.State
import Core.Context
import Core.Handler
import Data.Queue
import Data.Record
import GHC.TypeNats
import Interface.I2S
import Interface.I2SRX
import Ivory.Language

data I2SReceive n = I2SReceive
    { i2sI2SReceiveQueue :: Queue n (Records n SampleStruct)
    , i2sI2SReceiveSample :: Sample
    }

mkI2SReseive ::
    ( MonadState Context m
    , KnownNat n
    , Handler HandleI2SRX i
    ) =>
    i ->
    String ->
    m (I2SReceive n)
mkI2SReseive i2s name = do
    i2sI2SReceiveQueue <-
        queue (name <> "_i2s_receive_queue")
            =<< records_ (name <> "_i2s_buff_receive")
    i2sI2SReceiveSample <-
        record
            (name <> "_i2s_receive_sample")
            [left .= izero, right .= izero]

    let i2sReceive = I2SReceive{i2sI2SReceiveQueue, i2sI2SReceiveSample}

    addHandler $ HandleI2SRX i2s (receiveI2S i2sReceive)

    return i2sReceive

receiveI2S :: (KnownNat n) => I2SReceive n -> Sample -> Ivory eff ()
receiveI2S I2SReceive{..} sample =
    push i2sI2SReceiveQueue \i2sI2SReceiveBuff i -> do
        l <- deref (sample ~> left)
        r <- deref (sample ~> right)
        store (i2sI2SReceiveBuff ! toIx i ~> left) $ l `iDiv` 256
        store (i2sI2SReceiveBuff ! toIx i ~> right) $ r `iDiv` 256

getI2SReseiveSample :: (KnownNat n) => I2SReceive n -> Ivory eff Sample
getI2SReseiveSample I2SReceive{..} = do
    pop i2sI2SReceiveQueue \buff i -> do
        i2sI2SReceiveSample <== buff ! toIx i
    pure i2sI2SReceiveSample
