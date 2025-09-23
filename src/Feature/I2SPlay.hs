{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.I2SPlay where

import Control.Monad.State
import Core.Context
import Core.Handler
import Data.Queue
import Data.Record
import GHC.TypeLits
import Interface.I2S
import Interface.I2STX
import Ivory.Language

data I2SPlay n = I2SPlay
    { i2sTxQueue :: Queue n (Records n SampleStruct)
    , i2sTxSample :: Sample
    }

mkI2SPlay ::
    ( MonadState Context m
    , Handler HandleI2STX i
    , KnownNat n
    ) =>
    String ->
    i ->
    m (I2SPlay n)
mkI2SPlay name i2s = do
    i2sTxQueue <- queue (name <> "_i2s_tx_queue") =<< records_ (name <> "_i2s_buff_tx")
    i2sTxSample <- record (name <> "_i2s_tx_sample") [left .= izero, right .= izero]

    let i2stx = I2SPlay{i2sTxQueue, i2sTxSample}

    addHandler $ HandleI2STX i2s (transmitI2S i2stx)

    pure i2stx

transmitI2S ::
    (KnownNat n) =>
    I2SPlay n ->
    Ivory eff Sample
transmitI2S I2SPlay{..} = do
    pop i2sTxQueue \i2sBuff i -> do
        i2sTxSample <== i2sBuff ! i
    pure i2sTxSample

playI2S ::
    (KnownNat n) =>
    I2SPlay n ->
    (Sample -> Ivory eff ()) ->
    Ivory eff ()
playI2S I2SPlay{..} callback = push i2sTxQueue \buff i -> do
    callback (buff ! i)
