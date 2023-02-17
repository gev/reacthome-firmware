{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}

module Feature.RBUS    where

import           Control.Monad.Reader  (Reader, asks, runReader)
import           Core.Domain
import           Core.Feature
import           Core.Include
import           Core.Initialize
import           Data.Buffer
import           Data.Class
import           Data.Concurrent.Queue (Queue, pop, push, queue, size)
import           Data.Data             (cast)
import           Data.Index            (Index, index)
import           Data.Value
import           GHC.IO.BufferedIO     (readBuf)
import           GHC.TypeNats
import           Interface.Mac         (Mac (getMac))
import           Interface.MCU         (MCU)
import           Interface.RS485       as RS (HandleRS485 (HandleRS485), RS485,
                                              transmit)
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RBUS.Slave   (Slave, buffDisc, buffPing, hasAddress,
                                        receive, slave)
import qualified Protocol.RBUS.Slave   as RS


data RBUS = RBUS
    { name        :: String
    , rs          :: RS485
    , protocol    :: Slave   256
    , rxBuff      :: Buffer  64 Uint16
    , rxQueue     :: Queue   64
    , msgSize     :: Value      Uint16
    , msgSizeBuff :: Buffer  32 Uint16
    , msgQueue    :: Queue   32
    , msgIndex    :: Index      (Ix 512)
    , msgBuff     :: Buffer 512 Uint16
    , txBuff      :: Buffer 256 Uint16
    , txLock      :: Value      IBool
    }


rbus :: MCU mcu => Int -> Reader mcu RS485 -> Reader (Domain mcu) Feature
rbus n rs = do
    model       <- asks model
    version     <- asks version
    mac         <- asks mac
    mcu         <- asks mcu
    let handle   _ = pure ()
    let rbus     = RBUS { name          = name
                        , rs            = runReader rs mcu
                        , protocol      = slave name (getMac mac) model version handle
                        , rxBuff        = buffer (name <> "_rx")
                        , rxQueue       = queue  (name <> "_rx")
                        , msgSizeBuff   = buffer (name <> "_msg_size")
                        , msgQueue      = queue  (name <> "_msg")
                        , msgSize       = value  (name <> "_msg_size") 0
                        , msgBuff       = buffer (name <> "_msg_buffer")
                        , msgIndex      = index  (name <> "_msg_index")
                        , txLock        = value  (name <> "_tx_lock") false
                        , txBuff        = buffer (name <> "_tx")
                        }
    pure $ Feature rbus
    where name = "rbus_slave_" <> show n


instance Include RBUS where
    include r = do include $ rxBuff          r
                   include $ rxQueue         r
                   include $ msgSizeBuff     r
                   include $ msgQueue        r
                   include $ msgSize         r
                   include $ msgIndex        r
                   include $ msgBuff         r
                   include $ txLock          r
                   include $ txBuff          r
                   include $ HandleRS485 (rs r) (rxHandle r) (txHandle r)
                   include $ protocol        r


instance Initialize RBUS where
    initialize (RBUS {rs, protocol}) = initialize rs <> initialize protocol


instance Task RBUS where
    tasks r = [ yeld       (name r <> "_rx"  ) $ rxTask r
            --   , delay 10   (name r <> "_tx"  ) $ txTask r
              , delay 1000 (name r <> "_ping") $ pingTask r
              ]


txHandle :: RBUS -> Ivory eff ()
txHandle (RBUS {txLock}) = setValue txLock false


txTask :: RBUS -> Ivory (ProcEffects s ()) ()
txTask (RBUS {rs, msgIndex, msgSizeBuff, msgQueue, msgBuff, txBuff, txLock}) = do
    locked <- getValue txLock
    when (iNot locked) $ do
        pop msgQueue $ \i -> do
            size <- getItem msgSizeBuff (toIx i)
            for (toIx size) $ \dx -> do
                sx <- getValue msgIndex
                v <- getItem msgBuff sx
                setItem txBuff dx v
                setValue msgIndex $ sx + 1
            let buff = toCArray $ getBuffer txBuff
            transmit rs buff (size + 2)
            setValue txLock true




rxHandle :: RBUS -> Uint16 -> Ivory eff ()
rxHandle (RBUS {rxBuff, rxQueue}) value = do
    push rxQueue $ \i -> do
        setItem rxBuff (toIx i) value


rxTask :: RBUS -> Ivory (ProcEffects s ()) ()
rxTask (RBUS {rs, protocol, rxBuff, rxQueue, txBuff}) =
    pop rxQueue $ \i -> do
        v <- getItem rxBuff (toIx i)
        receive protocol $ castDefault v



pingTask :: RBUS -> Ivory (ProcEffects s ()) ()
pingTask r@(RBUS {protocol}) = do
    shouldPing <- hasAddress protocol
    ifte_ shouldPing
          (transmitPing r)
          (transmitDiscovery r)

transmitPing :: RBUS -> Ivory (ProcEffects s ()) ()
transmitPing (RBUS {protocol, rs, txBuff, txLock}) = do
    locked <- getValue txLock
    when (iNot locked) $ do
        let buff = buffPing protocol
        arrayMap $ \ix -> do
            v <- getItem buff (toIx . fromIx $ ix)
            setItem txBuff ix $ safeCast v
        let array = toCArray $ getBuffer txBuff
        transmit rs array $ getSize buff
        setValue txLock true

transmitDiscovery :: RBUS -> Ivory (ProcEffects s ()) ()
transmitDiscovery (RBUS {protocol, rs, txBuff, txLock}) = do
    locked <- getValue txLock
    when (iNot locked) $ do
        let buff = buffDisc protocol
        arrayMap $ \ix -> do
            v <- getItem buff (toIx . fromIx $ ix)
            setItem txBuff ix $ safeCast v
        let array = toCArray $ getBuffer txBuff
        transmit rs array $ getSize buff
        setValue txLock true
