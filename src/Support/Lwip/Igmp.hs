{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.Lwip.Igmp
    ( initIgmp
    , startIgmp
    , tmrIgmp
    , joinIgmpGroupNetif

    , inclIgmp
    ) where


import           Ivory.Language
import           Ivory.Language.Proc   (ProcType)
import           Ivory.Language.Syntax (Sym)
import           Ivory.Support
import           Support.Lwip.Err
import           Support.Lwip.IP_addr
import           Support.Lwip.Netif


fun :: ProcType f => Sym -> Def f
fun = funFrom "lwip/igmp.h"


initIgmp :: Ivory eff ()
initIgmp = call_ igmp_init

igmp_init :: Def ('[] :-> ())
igmp_init = fun "igmp_init"


startIgmp :: NETIF s -> Ivory eff ErrT
startIgmp = call igmp_start

igmp_start :: Def ('[NETIF s] :-> ErrT)
igmp_start = fun "igmp_start"


tmrIgmp :: Ivory eff ()
tmrIgmp = call_ igmp_tmr

igmp_tmr :: Def ('[] :-> ())
igmp_tmr = fun "igmp_tmr"


joinIgmpGroupNetif :: NETIF s -> IP_ADDR_4 s1 -> Ivory eff ErrT
joinIgmpGroupNetif = call igmp_joingroup_netif

igmp_joingroup_netif :: Def ('[NETIF s, IP_ADDR_4 s1] :-> ErrT)
igmp_joingroup_netif = fun "igmp_joingroup_netif"


inclIgmp :: ModuleDef
inclIgmp = do
    incl igmp_init
    incl igmp_start
    incl igmp_tmr
    incl igmp_joingroup_netif
