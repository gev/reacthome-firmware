{-# LANGUAGE DataKinds #-}

module Transport.RBUS.Data where

import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Interface.RS485
import           Interface.SystemClock
import           Ivory.Language
import           Protocol.RBUS.Slave


data RBUS = RBUS
    { name          :: String
    , clock         :: SystemClock
    , rs            :: RS485
    , protocol      :: Slave  255
    , rxBuff        :: Buffer  64 Uint16
    , rxQueue       :: Queue   64
    , msgOffset     :: Buffer  32 Uint16
    , msgSize       :: Buffer  32 Uint16
    , msgTTL        :: Buffer  32 Uint8
    , msgQueue      :: Queue   32
    , msgBuff       :: Buffer 512 Uint16
    , msgIndex      :: Value      Uint16
    , txBuff        :: Buffer 255 Uint16
    , initBuff      :: Buffer   1 Uint8
    , txLock        :: Value      IBool
    , timestamp     :: Value      Uint32
    , shouldConfirm :: Value      IBool
    , shouldInit    :: Value      IBool
    }
