{-# LANGUAGE QuasiQuotes   #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.Lwip.IP_addr
    ( IP_ADDR_4_STRUCT
    , IP_ADDR_4
    
    , ipAddrAny
    , createIpAddr4

    , inclIP_addr
    ) where


import           Ivory.Language
import           Ivory.Language.Proc   (ProcType)
import           Ivory.Language.Syntax (Sym)
import           Ivory.Support


fun :: ProcType f => Sym -> Def f
fun = funFrom "ip_addr.h"



type IP_ADDR_4_STRUCT = "ip4_addr"
type IP_ADDR_4 s = Ref s (Struct IP_ADDR_4_STRUCT)

[ivory|
    struct ip4_addr
        { addr :: Stored Uint32 }
|]



ipAddrAny :: IP_ADDR_4 Global
ipAddrAny = addrOf ip_addr_any

ip_addr_any :: MemArea (Struct IP_ADDR_4_STRUCT)
ip_addr_any = area "ip4_addr_any" $ Just $ istruct [ addr .= ival 0 ]



createIpAddr4 :: IP_ADDR_4 s -> Uint8 -> Uint8 -> Uint8 -> Uint8 -> Ivory eff ()
createIpAddr4 = call_ ip_addr4

ip_addr4 :: Def ('[IP_ADDR_4 s, Uint8, Uint8, Uint8, Uint8] :-> ())
ip_addr4 = fun "IP_ADDR4"


inclIP_addr :: ModuleDef
inclIP_addr = do
    incl ip_addr4
    defMemArea ip_addr_any
