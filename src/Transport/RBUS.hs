{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Transport.RBUS    where

import           Control.Monad.Reader  (Reader, asks, runReader)
import           Core.Context
import           Core.Dispatcher
import qualified Core.Domain           as D
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Concurrent.Queue
import           Data.Value
import           Interface.Mac         (getMac)
import           Interface.MCU         (MCU (peripherals, systemClock), mac)
import           Interface.RS485
import           Interface.SystemClock (getSystemTime)
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RBUS.Slave   (slave)
import           Transport.RBUS.Data
import           Transport.RBUS.Rx
import           Transport.RBUS.Tx


rbus :: Reader p RS485 -> Reader (D.Domain p t) RBUS
rbus rs = do
    model       <- asks D.model
    version     <- asks D.version
    mcu         <- asks D.mcu
    shouldInit  <- asks D.shouldInit
    features    <- asks D.features
    {--
        TODO: move dispatcher outside
    --}
    let dispatch = makeDispatcher features
    let rbus     = RBUS { name          = name
                        , clock         = systemClock mcu
                        , rs            = runReader rs $ peripherals mcu
                        , protocol      = slave name (getMac $ mac mcu) model version (onMessage dispatch rbus) (onConfirm rbus) (onDiscovery rbus)
                        , rxBuff        = buffer (name <> "_rx")
                        , rxQueue       = queue  (name <> "_rx")
                        , msgOffset     = buffer (name <> "_msg_offset")
                        , msgSize       = buffer (name <> "_msg_size")
                        , msgTTL        = buffer (name <> "_msg_ttl")
                        , msgQueue      = queue  (name <> "_msg")
                        , msgBuff       = buffer (name <> "_msg")
                        , msgIndex      = value  (name <> "_msg_index") 0
                        , txBuff        = buffer (name <> "_tx")
                        , initBuff      = values (name <> "_init_request") [0xf2]
                        , txLock        = value  (name <> "_tx_lock") false
                        , timestamp     = value  (name <> "_timestamp") 0
                        , shouldConfirm = value  (name <> "_should_confirm") false
                        , shouldInit    = shouldInit
                        }
    pure rbus

    where name = "rbus_slave"

          onMessage dispatch (RBUS {..}) buff n shouldHandle = do
            store (addrOf timestamp) =<< getSystemTime clock
            when shouldHandle $ dispatch buff n
            store (addrOf shouldConfirm) true

          {-
            TODO: Should make Init request here?
            TODO: Should reset Tx queue when address has changed?
          -}
          onDiscovery (RBUS {..}) = do
            store (addrOf timestamp) =<< getSystemTime clock
            store (addrOf shouldConfirm) false

          onConfirm = remove . msgQueue


instance Include RBUS where
    include r@(RBUS {..}) = do
      include rs
      include rxBuff
      include rxQueue
      include msgOffset
      include msgSize
      include msgTTL
      include msgQueue
      include msgBuff
      include msgIndex
      include txBuff
      include initBuff
      include txLock
      include timestamp
      include shouldConfirm
      include $ HandleRS485 rs (rxHandle r) (txHandle r)
      include protocol



instance Task RBUS where
    tasks r = [ yeld    (name r <> "_rx"  ) $ rxTask r
              , delay 1 (name r <> "_tx"  ) $ txTask r
              ]


instance Transport RBUS where
    transmit = toQueue
