{-# LANGUAGE DataKinds #-}

module Feature.RS485.RBUS.Data where
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Interface.RS485
import           Interface.SystemClock
import           Ivory.Language
import           Protocol.RS485.RBUS        (Preamble (confirm))
import           Protocol.RS485.RBUS.Master


data RBUS = RBUS
     { index            :: Int
     , clock            :: SystemClock
     , rs               :: RS485
     , protocol         :: Master 255
     , rxBuff           :: Buffer  64 Uint16
     , rxQueue          :: Queue   64
     , msgOffset        :: Buffer  32 Uint16
     , msgSize          :: Buffer  32 Uint16
     , msgTTL           :: Buffer  32 Uint8
     , msgQueue         :: Queue   32
     , msgConfirm       :: Values 255 IBool
     , msgBuff          :: Buffer 512 Uint16
     , msgIndex         :: Value      Uint16
     , txBuff           :: Buffer 255 Uint16
     , rxLock           :: Value      IBool
     , txLock           :: Value      IBool
     , timestamp        :: Value      Uint32
     , shouldDiscovery  :: Value      IBool
     , shouldConfirm    :: Value      IBool
     , shouldPing       :: Value      IBool
     , discoveryAddress :: Value      Uint8
     , confirmAddress   :: Value      Uint8
     , pingAddress      :: Value      Uint8
     }
