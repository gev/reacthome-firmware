{-# LANGUAGE DataKinds #-}

module Feature.RS485.RBUS.Data where
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Interface.RS485
import           Ivory.Language
import           Protocol.RS485.RBUS.Master


data RBUS = RBUS
     { n             :: Uint8
     , rs            :: RS485
     , protocol      :: Master 255
     , rxBuff        :: Buffer  64 Uint16
     , rxQueue       :: Queue   64
     , msgOffset     :: Buffer  32 Uint16
     , msgSize       :: Buffer  32 Uint16
     , msgTTL        :: Buffer  32 Uint8
     , msgQueue      :: Queue   32
     , msgBuff       :: Buffer 512 Uint16
     , msgIndex      :: Value      Uint16
     , txBuff        :: Buffer 255 Uint16
     , txLock        :: Value      IBool
     , timestamp     :: Value      Uint32
     , shouldConfirm :: Value      IBool
     }
