{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

module Transport.UDP.RBUS.Data where
import           Data.Buffer
import           Data.Record
import           Data.Value
import           Feature.RS485.RBUS.Data  (RBUS (shouldDiscovery))
import           Interface.Mac
import           Ivory.Language
import           Support.Lwip.IP_addr
import           Support.Lwip.Netif
import           Support.Lwip.Udp
import           Transport.UART.RBUS.Data (RBUS (txBuff))

data RBUS = RBUS
    { mac             :: Mac
    , netif           :: Record     NETIF_STRUCT
    , upcb            :: Value     (UDP_PCB Global)
    , hasIP           :: Value      IBool
    , serverIP        :: Record     IP_ADDR_4_STRUCT
    , serverPort      :: Value      Uint16
    , localIP         :: Record     IP_ADDR_4_STRUCT
    , netmask         :: Record     IP_ADDR_4_STRUCT
    , broadcastIP     :: Record     IP_ADDR_4_STRUCT
    , rxBuff          :: Buffer 256 Uint8
    , txBuff          :: Buffer 256 Uint8
    , discovery       :: Buffer   4 Uint8
    , requestIP       :: Buffer   1 Uint8
    , requestInit     :: Buffer   1 Uint8
    , shouldDiscovery :: Value      IBool
    , shouldInit      :: Value      IBool
    , onMessage       :: Buffer 256 Uint8 -> Uint8 -> forall s t. Ivory (ProcEffects s t) ()
    }
