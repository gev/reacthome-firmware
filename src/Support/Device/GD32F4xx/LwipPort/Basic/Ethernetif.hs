{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Support.Device.GD32F4xx.LwipPort.Basic.Ethernetif (
    initEthernetifPtr,
    inputEthernetif,
    inclEthernetif,
) where

import Ivory.Language
import Ivory.Language.Proc
import Ivory.Language.Syntax
import Ivory.Support
import Support.Lwip.Err
import Support.Lwip.Netif

fun :: (ProcType f) => Sym -> Def f
fun = funFrom "ethernetif.h"

initEthernetifPtr :: ProcPtr ('[NETIF s] :-> ErrT)
initEthernetifPtr = procPtr ethernetif_init

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
