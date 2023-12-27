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
     RBUS :: UART u
          => { name          :: String
             , model         :: Value       Uint8
             , version       :: Version
             , mac           :: Mac
             , clock         :: SystemClock
             , uart          :: u
             , protocol      :: U.RBUS  255
             , rxBuff        :: Buffer  512 Uint16
             , rxQueue       :: Queue   512
             , msgOffset     :: Buffer  256 Uint16
             , msgSize       :: Buffer  256 Uint16
             , msgQueue      :: Queue   256
             , msgBuff       :: Buffer 4096 Uint16
             , msgIndex      :: Value       Uint16
             , txBuff        :: Buffer  300 Uint16
             , discoveryBuff :: Buffer   10 Uint8
             , txLock        :: Value       IBool
             , rxTimestamp   :: Value       Uint32
             } -> RBUS
