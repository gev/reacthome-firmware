{-# HLINT ignore "Use camelCase" #-}

module Support.Lwip.Dhcp (
    startDhcp,
    suppliedAddressDhcp,
    coarseTmrDhcp,
    fineTmrDhcp,
    inclDhcp
) where

import Ivory.Language
import Ivory.Language.Proc (ProcType)
import Ivory.Language.Syntax (Sym)
import Ivory.Support
import Support.Lwip.Err
import Support.Lwip.Netif

fun :: (ProcType f) => Sym -> Def f
fun = funFrom "lwip/dhcp.h"

startDhcp :: NETIF s -> Ivory eff ErrT
startDhcp = call dhcp_start

dhcp_start :: Def ('[NETIF s] :-> ErrT)
dhcp_start = fun "dhcp_start"

suppliedAddressDhcp :: NETIF s -> Ivory eff IBool
suppliedAddressDhcp = call dhcp_supplied_address

dhcp_supplied_address :: Def ('[NETIF s] :-> IBool)
dhcp_supplied_address = fun "dhcp_supplied_address"

coarseTmrDhcp :: Ivory eff ()
coarseTmrDhcp = call_ dhcp_coarse_tmr

dhcp_coarse_tmr :: Def ('[] :-> ())
dhcp_coarse_tmr = fun "dhcp_coarse_tmr"

fineTmrDhcp :: Ivory eff ()
fineTmrDhcp = call_ dhcp_fine_tmr

dhcp_fine_tmr :: Def ('[] :-> ())
dhcp_fine_tmr = fun "dhcp_fine_tmr"

inclDhcp :: ModuleDef
inclDhcp = do
    incl dhcp_start
    incl dhcp_supplied_address
    incl dhcp_coarse_tmr
    incl dhcp_fine_tmr

