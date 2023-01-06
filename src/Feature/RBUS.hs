{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.RBUS    where

import           Device.GD32F3x0.SystemClock
import           Feature
import           GHC.TypeNats
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
    { name         :: String
    , rs           :: RS485
    , timestamp    :: Value      Uint32
    , rxBuff       :: Buffer  64 Uint16
    , rxQueue      :: Queue   64
    , msgStartBuff :: Buffer  16 Uint16
    , msgSizeBuff  :: Buffer  16 Uint16
    , msgQueue     :: Queue   16
    , msgStart     :: Value      Uint16
    , msgSize      :: Value      Uint16
    , msgBuff      :: Buffer 512 Uint16
    , txBuff       :: Buffer 256 Uint16
    , txLock       :: Value      IBool
    }


rbus :: Int -> RS485 -> Feature
rbus n rs = Feature $ RBUS
    { name         = name
    , rs           = rs
    , timestamp    = value  (name <> "_rx_timestamp") 0
    , rxBuff       = buffer (name <> "_rx")
    , rxQueue      = queue  (name <> "_rx")
    , msgStartBuff = buffer (name <> "_msg_start")
    , msgSizeBuff  = buffer (name <> "_msg_size")
    , msgQueue     = queue  (name <> "_msg")
    , msgStart     = value  (name <> "_msg_start") 0
    , msgSize      = value  (name <> "_msg_size") 0
    , msgBuff      = buffer (name <> "_msg_buffer")
    , txLock       = value  (name <> "_tx_lock") false
    , txBuff       = buffer (name <> "_tx")
    } where name = "rbus_" <> show n


instance Include RBUS where
    include (RBUS { rs
                  , timestamp
                  , rxBuff
                  , rxQueue
                  , msgStartBuff
                  , msgSizeBuff
                  , msgQueue
                  , msgStart
                  , msgSize
                  , msgBuff
                  , txLock
                  , txBuff
                  }) = do include timestamp
                          include rxBuff
                          include rxQueue
                          include msgStartBuff
                          include msgSizeBuff
                          include msgQueue
                          include msgStart
                          include msgSize
                          include msgBuff
                          include txLock
                          include txBuff
                          include $ HandleRS485 rs (onReceive timestamp rxQueue rxBuff)
                                                   (onTransmit txLock)


instance Initialize RBUS where
    initialize (RBUS {rs}) = initialize rs


instance Task RBUS where
    tasks (RBUS { name
                , rs
                , timestamp
                , rxBuff
                , rxQueue
                , msgStartBuff
                , msgSizeBuff
                , msgQueue
                , msgStart
                , msgSize
                , msgBuff
                , txBuff
                , txLock
                }) =
        [ yeld (name <> "_rx") $ do
            pop rxQueue $ \i -> do
                setItem msgBuff (toIx i) =<< getItem rxBuff (toIx i)
                size <- getValue msgSize
                when (size ==? 0) $
                    setValue msgStart i
                setValue msgSize $ size + 1
            t0 <- getValue timestamp
            t1 <- readCounter systemClock
            when (t1 - t0 >? 40) $ do
                size <- getValue msgSize
                when (size >? 0) $ do
                    push msgQueue $ \i -> do
                        let ix = toIx i
                        start <- getValue msgStart
                        setItem msgStartBuff ix start
                        setItem msgSizeBuff ix size
                        setValue msgSize 0

        , delay 1 (name <> "_tx") $ do
            locked <- getValue txLock
            when (iNot locked) $ do
                pop msgQueue $ \i -> do
                    let ix = toIx i
                    start <- getItem msgStartBuff ix
                    size <- getItem msgSizeBuff ix
                    for (toIx size) $ \ix ->
                        setItem txBuff ix =<< getItem msgBuff (toIx $ fromIx ix + safeCast start)
                    let tx = getBuffer txBuff
                    transmit rs (toCArray tx) size
                    setValue txLock true
        ]


onReceive :: KnownNat n
          => Value Uint32
          -> Queue n
          -> Buffer n Uint16
          -> Uint16
          -> Ivory eff ()
onReceive timestamp rxQueue rxBuff value = do
        push rxQueue $ \i -> do
            setItem rxBuff (toIx i) value
            setValue timestamp =<< readCounter systemClock


onTransmit :: Val v IBool
           => v IBool
           -> Ivory eff ()
onTransmit txLock =
    setValue txLock false
