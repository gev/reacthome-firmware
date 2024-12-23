{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Transport.UDP.RBUS.Data where
import           Data.Buffer
import           Data.Record
import           Data.Value
import           Feature.RS485.RBUS.Data (RBUS (shouldDiscovery))
import           Interface.Mac
import           Ivory.Language
import           Support.Lwip.IP_addr
import           Support.Lwip.Netif
import           Support.Lwip.Udp
import           Support.Lwip.Pbuf 
import Data.Concurrent.Queue

data RBUS = RBUS
    { mac             :: Mac
    , netif           :: Record      NETIF_STRUCT
    , upcb            :: Value      (UDP_PCB Global)
    , hasIP           :: Value       IBool
    , serverIP        :: Record      IP_ADDR_4_STRUCT
    , serverPort      :: Value       Uint16
    , localIP         :: Record      IP_ADDR_4_STRUCT
    , netmask         :: Record      IP_ADDR_4_STRUCT
    , broadcastIP     :: Record      IP_ADDR_4_STRUCT
    , rxMsgOffset     :: Value       Uint16
    , rxMsgOffsets    :: Values   40 Uint16
    , rxMsgSizes      :: Values   40 Uint8
    , rxMsgQueue      :: Queue    40
    , rxMsgBuff       :: Buffer 1200 Uint8
    , rxBuff          :: Buffer  255 Uint8
    , txBuff          :: Buffer  255 Uint8
    , discovery       :: Buffer    4 Uint8
    , requestIP       :: Buffer    1 Uint8
    , requestInit     :: Buffer    1 Uint8
    , shouldDiscovery :: Value       IBool
    , shouldInit      :: Value       IBool
    , onMessage       :: Buffer  255 Uint8 -> Uint8 -> forall s t. Ivory (ProcEffects s t) ()
    }
