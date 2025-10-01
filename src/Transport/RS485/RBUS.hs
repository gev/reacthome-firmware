module Transport.RS485.RBUS where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState, gets)
import Core.Context
import Core.Controller
import Core.Dispatcher
import Core.Domain qualified as D
import Core.Handler
import Core.Task
import Core.Transport
import Data.Buffer
import Data.Queue
import Data.Value
import Interface.MCU
import Interface.RS485
import Ivory.Language
import Ivory.Stdlib
import Protocol.RS485.RBUS
import Protocol.RS485.RBUS.Slave (slave)
import Transport.RS485.RBUS.Data
import Transport.RS485.RBUS.Rx
import Transport.RS485.RBUS.Tx

rbus ::
    (MonadState Context m, MonadReader (D.Domain p c) m, Controller c) =>
    m (RS485 256 300) ->
    m RBUS
rbus rs485 = do
    model <- asks D.model
    version <- asks D.version
    mcu <- asks D.mcu
    mustInit <- asks D.mustInit
    shouldInit <- asks D.shouldInit
    implementation <- asks D.implementation

    let name = "transport_rs485_rbus"
    let clock = systemClock mcu

    rs <- rs485
    msgQueue <- queue (name <> "_msg") =<< messages name
    msgBuff <- buffer (name <> "_msg")
    msgIndex <- value (name <> "_msg_index") 0
    initBuff <- values (name <> "_init_request") [0xf2]
    rxLock <- value (name <> "_rx_lock") false
    txLock <- value (name <> "_tx_lock") false
    rxTimestamp <- value (name <> "_timestamp_rx") 0
    txTimestamp <- value (name <> "_timestamp_tx") 0
    initTimestamp <- value (name <> "_timestamp_init") 0
    shouldConfirm <- value (name <> "_should_confirm") false
    msgConfirmed <- value (name <> "confirmed") false
    waitingConfirm <- value (name <> "_waiting_confirm") false

    {--
        TODO: move dispatcher outside
    --}
    let dispatch = makeDispatcher implementation

    let onMessage buff n shouldHandle = do
            when (n >? 0 .&& shouldHandle) do
                dispatch buff n
            store shouldConfirm true

    syncs <- gets getSyncs
    {-
      TODO: Should make Init request here?
      TODO: Should reset Tx queue when address has changed?
    -}
    let onDiscovery = do
            store shouldConfirm false
            store shouldInit mustInit
            mapM_ call_ syncs

    let onConfirm = do
            waitingConfirm' <- deref waitingConfirm
            when waitingConfirm' do
                store msgConfirmed true
                store waitingConfirm false

    let onReceive = store rxLock false

    protocol <-
        slave
            name
            (mac mcu)
            model
            version
            onMessage
            onConfirm
            onDiscovery
            onReceive

    let rbus =
            RBUS
                { clock
                , rs
                , protocol
                , msgQueue
                , msgBuff
                , msgIndex
                , initBuff
                , rxLock
                , txLock
                , rxTimestamp
                , txTimestamp
                , initTimestamp
                , shouldConfirm
                , msgConfirmed
                , waitingConfirm
                , shouldInit
                }

    addHandler $ HandleRS485 rs (rxHandle rbus) (txHandle rbus) (errorHandle rbus)

    addInit name $ configureRS485 rs defaultBaudrate WL_8b SB_1b None

    addTask $ yeld (name <> "_rx") $ rxTask rbus
    addTask $ yeld (name <> "_tx") $ txTask rbus
    addTask $ yeld (name <> "_reset") $ resetTask rbus
    addTask $ delay 2000 (name <> "_init") $ initTask rbus
    {-
        TODO: Move initialization out of the RBUS protocol
    -}

    pure rbus

instance Transport RBUS where
    transmitBuffer = toQueue

instance LazyTransport RBUS where
    lazyTransmit = toQueue'
