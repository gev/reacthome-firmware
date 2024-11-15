{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Transport.UART.RBUS.Data where

import           Core.Version          (Version)
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Interface.Mac
import           Interface.MCU         (MCU (mac))
import           Interface.SystemClock
import           Interface.UART
import           Ivory.Language
import qualified Protocol.UART.RBUS    as U


data RBUS where
     RBUS :: UART (u 300)
          => { name          :: String
             , speed         :: Uint32
             , model         :: Value       Uint8
             , version       :: Version
             , mac           :: Mac
             , clock         :: SystemClock
             , uart          :: u       300
             , protocol      :: U.RBUS  255
             , rxBuff        :: Buffer  300 Uint8
             , rxQueue       :: Queue   300
             , msgOffset     :: Buffer  128 Uint16
             , msgSize       :: Buffer  128 Uint8
             , msgQueue      :: Queue   128
             , msgBuff       :: Buffer 1200 Uint8
             , msgIndex      :: Value       Uint16
             , discoveryBuff :: Buffer   10 Uint8
             , txLock        :: Value       IBool
             , rxTimestamp   :: Value       Uint32
             } -> RBUS
