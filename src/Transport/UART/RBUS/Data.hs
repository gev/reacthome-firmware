{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Transport.UART.RBUS.Data where

import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Interface.SystemClock
import           Interface.UART
import           Ivory.Language
import qualified Protocol.UART.RBUS    as U


data RBUS where
     RBUS :: UART u
          => { name          :: String
             , clock         :: SystemClock
             , uart          :: u
             , protocol      :: U.RBUS  255
             , rxBuff        :: Buffer  300 Uint8
             , rxQueue       :: Queue   300
             , msgOffset     :: Buffer   64 Uint16
             , msgSize       :: Buffer   64 Uint8
             , msgQueue      :: Queue    64
             , msgBuff       :: Buffer 1024 Uint8
             , msgIndex      :: Value       Uint16
             , txBuff        :: Buffer  300 Uint16
             , txLock        :: Value       IBool
             , rxTimestamp   :: Value       Uint32
             } -> RBUS
