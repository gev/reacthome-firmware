{-# HLINT ignore "Use for_" #-}

module Transport.UDP.RBUS where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Context
import Core.Controller
import Core.Dispatcher
import Core.Domain qualified as D
import Core.Meta
import Core.Task
import Core.Transport
import Data.Buffer
import Data.Record
import Data.Value
import Interface.ENET
import Interface.LwipPort
import Interface.MCU
import Ivory.Language
import Ivory.Stdlib
import Protocol.RBUS (rbusDummy, rbusVersion)
import Support.Lwip.IP_addr
import Support.Lwip.Udp
import Transport.UDP.RBUS.Data
import Transport.UDP.RBUS.Rx
import Transport.UDP.RBUS.Tx
import Core.Actions

rbus ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , Enet e
    , LwipPort e
    , Controller c
    ) =>
    (p -> m e) ->
    m RBUS
rbus enet = do
    meta <- asks D.meta
    platform <- platform meta.mcu
    shouldInit <- asks D.shouldInit
    implementation <- asks D.implementation
    upcb <- value_ "udp_rbus_upcb"
    netif <- mkNetif enet
    serverIP <- record_ "udp_rbus_server_ip"
    serverPort <- value "udp_rbus_server_port" 2017
    localIP <- record_ "udp_rbus_local_ip"
    netmask <- record_ "udp_rbus_netmask"
    broadcastIP <- record_ "udp_rbus_broadcast_ip"
    hasIP <- value "udp_rbus_has_ip" false
    rxBuff <- buffer "udp_rbus_rx"
    txBuff <- buffer "udp_rbus_tx"
    discovery <- buffer "udp_rbus_discovery"
    requestIP <- buffer "udp_rbus_request_ip"
    requestInit <- buffer "udp_rbus_request_init"
    shouldDiscovery <- value "udp_rbus_should_discovery" false

    {--
        TODO: move dispatcher outside
    --}
    let onMessage = makeDispatcher implementation

    let rbus =
            RBUS
                { mac = platform.mac
                , netif
                , upcb
                , serverIP
                , serverPort
                , localIP
                , netmask
                , broadcastIP
                , hasIP
                , rxBuff
                , txBuff
                , discovery
                , requestIP
                , requestInit
                , shouldDiscovery
                , shouldInit
                , onMessage
                }

    addProc $ receiveCallback rbus

    addNetifOnUpCallback $ netifStatusCallback rbus

    addInit "udp_rbus_init" do
        createIpAddr4 serverIP 255 255 255 255
        createIpAddr4 broadcastIP 255 255 255 255

        store (discovery ! 0) actionDiscovery
        store (discovery ! 1) rbusDummy
        store (discovery ! 2) (fst rbusVersion)
        store (discovery ! 3) (snd rbusVersion)

        store (requestIP ! 0) actionIpAddress

        store (requestInit ! 0) actionInitialize

    addTask $ yeld "udp_rbus_discovery" $ discoveryTask rbus
    addTask $ delay 2_000 "request_init" $ requestInitTask rbus

    pure rbus

discoveryTask :: RBUS -> Ivory (ProcEffects s t) ()
discoveryTask rbus@RBUS{..} = do
    shouldDiscovery' <- deref shouldDiscovery
    when shouldDiscovery' do
        hasIP' <- deref hasIP
        ifte_
            hasIP'
            do transmit rbus discovery
            do broadcast rbus requestIP
        store shouldDiscovery false

requestInitTask :: RBUS -> Ivory (ProcEffects s t) ()
requestInitTask rbus@RBUS{..} = do
    hasIP' <- deref hasIP
    shouldInit' <- deref shouldInit
    when (hasIP' .&& shouldInit') do
        transmit rbus requestInit

netifStatusCallback :: RBUS -> Ivory (ProcEffects s ()) ()
netifStatusCallback rbus@RBUS{upcb} = do
    upcb' <- newUdp
    store upcb upcb'
    when (upcb' /=? nullPtr) do
        err <- bindUdp upcb' ipAddrAny 2017
        when (err ==? 0) do
            recvUdp upcb' (procPtr $ receiveCallback rbus) nullPtr

instance Transport RBUS where
    transmitBuffer = transmit

instance LazyTransport RBUS where
    lazyTransmit = lazyTransmit'
