{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}


module Transport.RBUS    where

import           Control.Monad.Reader  (Reader, asks, runReader)
import           Core.Dispatcher
import           Core.Domain
import           Core.Include
import           Core.Initialize
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Interface.Mac         (getMac)
import           Interface.MCU         (MCU (systemClock))
import           Interface.RS485
import           Interface.SystemClock (getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RBUS.Slave   (slave)
import           Transport.RBUS.Data
import           Transport.RBUS.Rx
import           Transport.RBUS.Tx


rbus :: MCU mcu => Reader mcu RS485 -> Reader (Domain mcu t) RBUS
rbus rs = do
    model       <- asks model
    version     <- asks version
    mac         <- asks mac
    mcu         <- asks mcu
    features    <- asks features
    {--
        TODO: move dispatcher outside
    --}
    let dispatch = makeDispatcher features
    let rbus     = RBUS { name          = name
                        , clock         = systemClock mcu
                        , rs            = runReader rs mcu
                        , protocol      = slave name (getMac mac) model version (onMessage dispatch rbus) (onConfirm rbus) (onDiscovery rbus)
                        , rxBuff        = buffer (name <> "_rx")
                        , rxQueue       = queue  (name <> "_rx")
                        , msgOffset     = buffer (name <> "_msg_offset")
                        , msgSize       = buffer (name <> "_msg_size")
                        , msgTTL        = buffer (name <> "_msg_ttl")
                        , msgQueue      = queue  (name <> "_msg")
                        , msgBuff       = buffer (name <> "_msg_buffer")
                        , msgIndex      = value  (name <> "_msg_index") 0
                        , txBuff        = buffer (name <> "_tx")
                        , txLock        = value  (name <> "_tx_lock") false
                        , timestamp     = value  (name <> "_timestamp") 0
                        , shouldConfirm = value  (name <> "_should_confirm") false
                        }
    pure rbus

    where name = "rbus_slave"

          onMessage dispatch (RBUS {shouldConfirm, clock, timestamp}) buff n shouldHandle = do
            store (addrOf timestamp) =<< getSystemTime clock
            when shouldHandle $ dispatch buff n
            store (addrOf shouldConfirm) true

          onDiscovery (RBUS {shouldConfirm, clock, timestamp}) = do
            store (addrOf timestamp) =<< getSystemTime clock
            store (addrOf shouldConfirm) false

          onConfirm = remove . msgQueue


instance Include RBUS where
    include r = do include $ rxBuff          r
                   include $ rxQueue         r
                   include $ msgOffset       r
                   include $ msgSize         r
                   include $ msgTTL          r
                   include $ msgQueue        r
                   include $ msgBuff         r
                   include $ msgIndex        r
                   include $ txBuff          r
                   include $ txLock          r
                   include $ timestamp       r
                   include $ shouldConfirm   r
                   include $ HandleRS485 (rs r) (rxHandle r) (txHandle r)
                   include $ protocol        r


instance Initialize RBUS where
    initialize (RBUS {rs, protocol}) = initialize rs <> initialize protocol


instance Task RBUS where
    tasks r = [ yeld    (name r <> "_rx"  ) $ rxTask r
              , delay 1 (name r <> "_tx"  ) $ txTask r
              ]


instance Transport RBUS where
    transmit = toQueue
