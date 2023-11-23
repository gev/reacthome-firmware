{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Lwip.Ethernet 
    ( inputEthernetPtr
    
    , inclEthernet  
    ) where
        
import Ivory.Language
import Ivory.Language.Syntax
import Ivory.Language.Proc
import Support.Lwip.Pbuf
import Support.Lwip.Netif
import Support.Lwip.Err
import Ivory.Support


fun :: ProcType f => Sym -> Def f
fun = funFrom "netif/ethernet.h"


inputEthernetPtr :: ProcPtr ('[PBUF s1, NETIF s2] :-> ErrT)
inputEthernetPtr = procPtr ethernet_input

ethernet_input :: Def ('[PBUF s1, NETIF s2] :-> ErrT)
ethernet_input = fun "ethernet_input"


inclEthernet = do
    incl ethernet_input
