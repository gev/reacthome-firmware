{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Transport.UART.RBUS.Data where

import           Core.Version          (Version)
import           Data.Buffer
import           Data.Queue
import           Data.Value
import           Interface.Mac
import           Interface.MCU         
import           Interface.SystemClock
import           Interface.UART
import           Ivory.Language
import qualified Protocol.UART.RBUS    as U
import GHC.TypeNats


data RBUS q l where
     RBUS :: (UART (u rn tn), KnownNat rn, KnownNat tn)
          => { name          :: String
             , speed         :: Uint32
             , model         :: Value       Uint8
             , version       :: Version
             , mac           :: Mac
             , clock         :: SystemClock
             , uart          :: u        rn tn
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
