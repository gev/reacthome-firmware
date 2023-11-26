module Transport.UDP.RBUS.Data where
import           Data.Record
import           Support.Lwip.IP_addr
import           Support.Lwip.Netif   (NETIF_STRUCT)

data RBUS = RBUS
    { netif     :: Record NETIF_STRUCT
    }
