{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.RBUS    where

import           Device.GD32F3x0.SystemClock
import           Feature
import           Include
import           Initialize
import           Interface.Counter           (readCounter)
import           Interface.RS485             (HandleRS485 (HandleRS485), RS485,
                                              transmit)
import           Ivory.Language
import           Ivory.Stdlib
import           Util.Data.Buffer
import           Util.Data.Class
import           Util.Data.Concurrent.Queue
import           Util.Data.Value


data RBUS = RBUS
    { name      :: String
    , rs        :: RS485
    , timestamp :: Value      Uint32
    , sizeTx    :: Value      Sint32
    , lockTx    :: Value      IBool
    , buffTx    :: Buffer 512 Uint16
    , buffRx    :: Buffer 512 Uint16
    , queueRx   :: Queue  512
    }


rbus :: Int -> RS485 -> Feature
rbus n rs = Feature $ RBUS
    { name      = name
    , rs        = rs
    , timestamp = value  (name <> "_rx_timestamp") 0
    , sizeTx    = value  (name <> "_tx_size") 0
    , lockTx    = value  (name <> "_tx_lock") false
    , buffTx    = buffer (name <> "_tx")
    , buffRx    = buffer (name <> "_rx")
    , queueRx   = queue  (name <> "_rx")
    } where name = "usart_" <> show n


instance Include RBUS where
    include (RBUS {rs, timestamp, sizeTx, lockTx, buffTx, buffRx, queueRx}) = do
        include timestamp
        include sizeTx
        include lockTx
        include buffTx
        include buffRx
        include queueRx
        include $ HandleRS485 rs (onReceive timestamp queueRx buffRx)
                                 (onTransmit lockTx)


instance Initialize RBUS where
    initialize (RBUS {rs}) = initialize rs


instance Task RBUS where
    tasks (RBUS {name, rs, timestamp, sizeTx, buffRx, buffTx, queueRx, lockTx}) =
        [
            yeld (name <> "_rx") $ do
                l <- getValue lockTx
                when (iNot l) $ do
                    pop queueRx $ \ix -> do
                        size <- getValue sizeTx
                        getItem buffRx ix >>= setItem buffTx (toIx size)
                        setValue sizeTx $ size + 1

        , delay 1 (name <> "_tx") $ do
                l <- getValue lockTx
                when (iNot l) $ do
                    t0 <- getValue timestamp
                    t1 <- readCounter systemClock
                    when (t1 - t0 >? 40) $ do
                        size <- getValue sizeTx
                        when (size >? 0) $ do
                            setValue sizeTx 0
                            setValue lockTx true
                            let tx = getBuffer buffTx
                            transmit rs (toCArray tx) size
        ]


onReceive timestamp queueRx buffRx b = do
        push queueRx $ \ix -> do
            setItem buffRx ix b
            setValue timestamp =<< readCounter systemClock


onTransmit lockTx =
    setValue lockTx false
