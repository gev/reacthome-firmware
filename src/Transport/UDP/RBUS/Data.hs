module Transport.UDP.RBUS.Data where
import Support.Lwip.Netif (NETIF_STRUCT)
import Data.Record
import Support.Lwip.IP_addr

data RBUS = RBUS 
    { netif     :: Record NETIF_STRUCT
    , igmpGroup :: Record IP_ADDR_4_STRUCT
    }
