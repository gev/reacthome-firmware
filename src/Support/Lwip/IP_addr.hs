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
fun = funFrom "lwip/ip_addr.h"


type IP_ADDR_4_T = "ip4_addr"
type IP_ADDR_4 s = Ref s (Struct IP_ADDR_4_T)

[ivory|
    abstract struct ip4_addr "ip_addr.h"
|]

-- IP_ADDR_4 s $ ext "IP_ADDR_ANY" 

createIpAddr4 :: IP_ADDR_4 s -> Uint8 -> Uint8 -> Uint8 -> Uint8 -> Ivory eff ()
createIpAddr4 = call_ ip_addr4

ip_addr4 :: Def ('[IP_ADDR_4 s, Uint8, Uint8, Uint8, Uint8] :-> ())
ip_addr4 = fun "IP_ADDR4"


inclIP_addr :: ModuleDef
inclIP_addr = do
    incl ip_addr4
