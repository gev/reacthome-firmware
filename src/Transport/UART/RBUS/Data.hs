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


data RBUS q l where
     RBUS :: UART (u 32 300)
          => { name          :: String
             , speed         :: Uint32
             , model         :: Value       Uint8
             , version       :: Version
             , mac           :: Mac
             , clock         :: SystemClock
             , uart          :: u        32 300
             , protocol      :: U.RBUS  255
             , msgOffset     :: Buffer    q Uint16
             , msgSize       :: Buffer    q Uint8
             , msgQueue      :: Queue     q
             , msgBuff       :: Buffer    l Uint8
             , msgIndex      :: Value       Uint16
             , discoveryBuff :: Buffer   10 Uint8
             , txLock        :: Value       IBool
             , rxTimestamp   :: Value       Uint32
             } -> RBUS q l
