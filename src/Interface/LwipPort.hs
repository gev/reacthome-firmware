{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interface.LwipPort where

import Control.Monad (void)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Core.Context
import Core.Controller
import qualified Core.Domain as D
import Core.Handler
import Core.Task
import Data.Record
import Data.Value
import Interface.ENET
import Interface.MCU
import qualified Interface.MCU as I
import Ivory.Language
import Ivory.Stdlib
import Support.Lwip.Err
import Support.Lwip.Etharp
import Support.Lwip.Ethernet
import Support.Lwip.IP_addr
import Support.Lwip.Igmp
import Support.Lwip.Mem
import Support.Lwip.Memp
import Support.Lwip.Netif
import Support.Lwip.Pbuf
import Support.Lwip.Udp

mkNetif ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , Enet e
    , LwipPort e
    ) =>
    (p -> m e) ->
    m (Record NETIF_STRUCT)
mkNetif enet' = do
    mcu <- asks D.mcu
    let mac = I.mac mcu
    enet <- enet' $ peripherals mcu
    netif <- record_ "netif"
    localIP <- record_ "local_ip"
    netmask <- record_ "netmask"

    addModule inclEthernet
    addModule inclNetif
    addModule inclUdp
    addModule inclMem
    addModule inclMemp
    addModule inclIP_addr
    addModule inclPbuf
    addModule inclEtharp
    addModule inclIgmp

    let netifStatusCallback = proc "netif_callback" $ \netif -> body $ do
            flags' <- deref $ netif ~> flags
            when (flags' .& netif_flag_up /=? 0) $
                call_ netifOnUpCallback

    addProc netifStatusCallback

    addInit "netif_init" $ do
        initMem
        initMemp

        createIpAddr4 localIP 169 254 47 94
        createIpAddr4 netmask 255 255 0 0

        store (netif ~> hwaddr_len) 6

        arrayMap $ \ix -> do
            m <- deref (mac ! ix)
            store (netif ~> hwaddr ! ix) m

        addNetif
            netif
            localIP
            netmask
            ipAddrAny
            nullPtr
            (initLwipPortIf enet)
            inputEthernetPtr

        setNetifDefault netif
        setNetifStatusCallback netif $ procPtr netifStatusCallback

        initIgmp

        startIgmp netif
        setUpNetif netif

    addTask $ yeld "udp_rx" $ rxTask enet netif
    addTask $ delay 1_000 "tmr_arp" tmrEtharp
    addTask $ delay 100 "tmr_igmp" tmrIgmp

    pure netif

netifOnUpCallback :: Def ('[] :-> ())
netifOnUpCallback = proc "netif_on_up_callback" $ body $ pure ()

addNetifOnUpCallback ::
    (MonadState Context m) =>
    (forall s. Ivory (ProcEffects s ()) ()) ->
    m ()
addNetifOnUpCallback = addBody "netif_on_up_callback"

rxTask ::
    (LwipPort e, Enet e) =>
    e ->
    NETIF s1 ->
    Ivory (ProcEffects s2 ()) ()
rxTask enet netif = do
    reval <- rxFrameSize enet
    when (reval >? 1) $
        void $
            inputLwipPortIf enet netif

class LwipPort p where
    initLwipPortIf :: p -> ProcPtr ('[NETIF s] :-> ErrT)
    inputLwipPortIf :: p -> NETIF s -> Ivory eff ErrT
