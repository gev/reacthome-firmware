{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Support.Lwip.IP_addr
    ( IP_ADDR_4
    , IP_ADDR_4_POINTER

    , createIpAddr4

    , inclIP_addr
    ) where

import           Ivory.Language
import           Ivory.Support
import           Ivory.Language.Proc
import           Ivory.Language.Syntax


fun :: ProcType f => Sym -> Def f
fun = funFrom "lwip/ip_addr.h"


type IP_ADDR_4_T = "ip_addr_t"
type IP_ADDR_4 s = Ref s IP_ADDR_4_T


createIpAddr4 :: IP_ADDR_4 s -> Uint8 -> Uint8 -> Uint8 -> Uint8 -> Ivory eff ()
createIpAddr4 = call_ ip_addr4

ip_addr4 :: Def ('[IP_ADDR_4 s, Uint8, Uint8, Uint8, Uint8] :-> ())
ip_addr4 = fun "IP_ADDR4"


inclIP_addr :: ModuleDef
inclIP_addr = do
    incl ip_addr4
