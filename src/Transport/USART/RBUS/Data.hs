{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Transport.USART.RBUS.Data where

import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Interface.SystemClock
import           Interface.USART
import           Ivory.Language
import qualified Protocol.USART.RBUS   as U


data RBUS where
     RBUS :: USART u
          => { name          :: String
             , clock         :: SystemClock
             , usart         :: u
             , protocol      :: U.RBUS   255
             , rxBuff        :: Buffer   64 Uint16
             , rxQueue       :: Queue    64
             , msgOffset     :: Buffer   32 Uint16
             , msgSize       :: Buffer   32 Uint16
             , msgQueue      :: Queue    32
             , msgBuff       :: Buffer 1024 Uint16
             , msgIndex      :: Value       Uint16
             , txBuff        :: Buffer 1024 Uint16
             , txLock        :: Value       IBool
             } -> RBUS
