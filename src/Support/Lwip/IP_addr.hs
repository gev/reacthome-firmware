{-# LANGUAGE QuasiQuotes       #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Support.Lwip.IP_addr
    ( IP_ADDR_4_T
    , IP_ADDR_4

    , createIpAddr4

    , inclIP_addr
    ) where




import           Ivory.Language
import           Ivory.Support
import           Ivory.Language.Proc   (ProcType)
import           Ivory.Language.Syntax (Sym)


fun :: ProcType f => Sym -> Def f
fun = funFrom "ip_addr.h"



type IP_ADDR_4_T = "ip4_addr"
type IP_ADDR_4 s = Ref s (Struct IP_ADDR_4_T)

[ivory|
    struct ip4_addr 
        { addr :: Stored Uint32 }
|]


ip_addr_any :: [InitStruct  IP_ADDR_4_T]
             -> Init (Struct IP_ADDR_4_T)
ip_addr_any p =
    istruct $ p <+> [ addr .= ival 0 ]



createIpAddr4 :: IP_ADDR_4 s -> Uint8 -> Uint8 -> Uint8 -> Uint8 -> Ivory eff ()
createIpAddr4 = call_ ip_addr4

ip_addr4 :: Def ('[IP_ADDR_4 s, Uint8, Uint8, Uint8, Uint8] :-> ())
ip_addr4 = fun "IP_ADDR4"


inclIP_addr :: ModuleDef
inclIP_addr = do
    incl ip_addr4
