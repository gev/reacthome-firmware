{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Support.Device.GD32F4xx.Lwip_port.Basic.Ethernetif   
    ( initEthernetif
    , inputEthernetif
    , inclEthernetif
    ) where


import  Support.Lwip.Netif
import  Support.Lwip.Err
import Ivory.Language
import Ivory.Language.Proc
import Ivory.Language.Syntax
import Ivory.Support


fun :: ProcType f => Sym -> Def f
fun = funFrom "ethernetif.h"


initEthernetif :: NETIF s -> Ivory eff ErrT
initEthernetif = call ethernetif_init

ethernetif_init :: Def ('[NETIF s] :-> ErrT)
ethernetif_init = fun "ethernetif_init"


inputEthernetif :: NETIF s -> Ivory eff ErrT
inputEthernetif = call ethernetif_input

ethernetif_input :: Def ('[NETIF s] :-> ErrT)
ethernetif_input = fun "ethernetif_input"



inclEthernetif :: ModuleDef
inclEthernetif = do 
    incl ethernetif_init
    incl ethernetif_input
