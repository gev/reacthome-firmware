{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Feature.RS485.RBUS.Data where

import           Core.Transport
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Endpoint.ATS                (ATS (payload))
import           Endpoint.DInputsRelaysRules (Rules (synced))
import           Interface.RS485
import           Interface.SystemClock
import           Ivory.Language
import           Protocol.RS485.RBUS         (Preamble (confirm))
import           Protocol.RS485.RBUS.Master



modeNone  = 2 :: Uint8
modeRBUS  = 1 :: Uint8
modeRS485 = 0 :: Uint8



data RBUS = forall t. (LazyTransport t, Transport t) => RBUS
     { index            :: Int
     , clock            :: SystemClock
     , rs               :: RS485   300
     , mode             :: Value       Uint8
     , baudrate         :: Value       Uint32
     , lineControl      :: Value       Uint8
     , protocol         :: Master  255 
     , rxBuff           :: Buffer   64 Uint16
     , rxQueue          :: Queue    64
     , msgOffset        :: Buffer   64 Uint16
     , msgSize          :: Buffer   64 Uint16
     , msgTTL           :: Buffer   64 Uint8
     , msgQueue         :: Queue    64
     , msgConfirm       :: Values  255 IBool
     , msgBuff          :: Buffer 1024 Uint16
     , msgIndex         :: Value       Uint16
     , txBuff           :: Buffer  255 Uint16
     , rsBuff           :: Buffer  253 Uint8
     , rsSize           :: Value       Uint8
     , rxLock           :: Value       IBool
     , txLock           :: Value       IBool
     , rxTimestamp      :: Value       Uint32
     , txTimestamp      :: Value       Uint32
     , shouldDiscovery  :: Value       IBool
     , shouldConfirm    :: Value       IBool
     , shouldPing       :: Value       IBool
     , discoveryAddress :: Value       Uint8
     , confirmAddress   :: Value       Uint8
     , pingAddress      :: Value       Uint8
     , shouldInit       :: Value       IBool
     , synced           :: Value       IBool
     , payload          :: Buffer    8 Uint8
     , transport        :: t
     }
