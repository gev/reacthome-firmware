{-# LANGUAGE DataKinds #-}

module Transport.RS485.RBUS.Data where

import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Interface.RS485
import           Interface.SystemClock
import           Ivory.Language
import           Protocol.RS485.RBUS.Slave


data RBUS = RBUS
    { clock         :: SystemClock
    , rs            :: RS485
    , protocol      :: Slave   255
    , rxBuff        :: Buffer  300 Uint8
    , rxQueue       :: Queue   300
    , msgOffset     :: Buffer   32 Uint16
    , msgSize       :: Buffer   32 Uint8
    , msgTTL        :: Buffer   32 Uint8
    , msgQueue      :: Queue    32
    , msgBuff       :: Buffer  600 Uint8
    , msgIndex      :: Value       Uint16
    , txBuff        :: Buffer  300 Uint16
    , initBuff      :: Buffer    1 Uint8
    , rxLock        :: Value       IBool
    , txLock        :: Value       IBool
    , rxTimestamp   :: Value       Uint32
    , txTimestamp   :: Value       Uint32
    , initTimestamp :: Value       Uint32
    , shouldConfirm :: Value       IBool
    , shouldInit    :: Value       IBool
    }
