{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use for_" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Transport.RBUS    where

import           Control.Monad.Reader  (Reader, asks, runReader)
import           Core.Dispatcher       (makeDispatcher)
import           Core.Domain
import           Core.Feature
import           Core.Include
import           Core.Initialize
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Class
import           Data.Concurrent.Queue (Queue, peek, pop, push, queue, remove,
                                        size)
import           Data.Index            (Index, index)
import           Data.Value
import           GHC.TypeNats
import qualified Interface.Counter     as I
import           Interface.Mac         (Mac (getMac))
import           Interface.MCU         (MCU (systemClock))
import           Interface.RS485       as RS (HandleRS485 (HandleRS485), RS485,
                                              transmit)
import qualified Interface.SystemClock as I
import           Ivory.Language
import           Ivory.Stdlib
import           Protocol.RBUS         (messageTTL)
import           Protocol.RBUS.Slave   (Slave, buffDisc, buffPing, hasAddress,
                                        receive, slave, transmitConfirm,
                                        transmitDiscovery, transmitMessage,
                                        transmitPing)
import qualified Protocol.RBUS.Slave   as RS


data RBUS = RBUS
    { name      :: String
    , rs        :: RS485
    , protocol  :: Slave   255
    , rxBuff    :: Buffer  64 Uint16
    , rxQueue   :: Queue   64
    , msgSize   :: Buffer  32 Uint16
    , msgTTL    :: Buffer  32 Uint8
    , msgQueue  :: Queue   32
    , msgBuff   :: Buffer 512 Uint16
    , msgOffset :: Value      Uint16
    , txBuff    :: Buffer 255 Uint16
    , txLock    :: Value      IBool
    }


rbus :: MCU mcu => Reader mcu RS485 -> Reader (Domain mcu t) RBUS
rbus rs = do
    model       <- asks model
    version     <- asks version
    mac         <- asks mac
    mcu         <- asks mcu
    features    <- asks features
    let clock    = systemClock mcu
    {--
        TODO: move dispatcher outside
    --}
    let dispatch = makeDispatcher features
    let rbus     = RBUS { name          = name
                        , rs            = runReader rs mcu
                        , protocol      = slave name (getMac mac) model version (onMessage clock dispatch rbus) (onConfirm rbus)
                        , rxBuff        = buffer (name <> "_rx")
                        , rxQueue       = queue  (name <> "_rx")
                        , msgSize       = buffer (name <> "_msg_size")
                        , msgTTL        = buffer (name <> "_msg_ttl")
                        , msgQueue      = queue  (name <> "_msg")
                        , msgBuff       = buffer (name <> "_msg_buffer")
                        , msgOffset     = value  (name <> "_msg_offset") 0
                        , txBuff        = buffer (name <> "_tx")
                        , txLock        = value  (name <> "_tx_lock") false
                        }
    pure rbus
    where name = "rbus_slave"

          onMessage clock dispatch rbus buff n = do
            confirm rbus
            I.delay clock 100
            dispatch buff n

          onConfirm = remove . msgQueue


instance Include RBUS where
    include r = do include $ rxBuff          r
                   include $ rxQueue         r
                   include $ msgSize         r
                   include $ msgTTL          r
                   include $ msgQueue        r
                   include $ msgBuff         r
                   include $ msgOffset       r
                   include $ txBuff          r
                   include $ txLock          r
                   include $ HandleRS485 (rs r) (rxHandle r) (txHandle r)
                   include $ protocol        r


instance Initialize RBUS where
    initialize (RBUS {rs, protocol}) = initialize rs <> initialize protocol


instance Task RBUS where
    tasks r = [ yeld       (name r <> "_rx"  ) $ rxTask r
              , delay 10   (name r <> "_tx"  ) $ txTask r
              , delay 1000 (name r <> "_ping") $ pingTask r
              ]


txHandle :: RBUS -> Ivory eff ()
txHandle (RBUS {txLock}) = setValue txLock false


txTask :: RBUS -> Ivory (ProcEffects s ()) ()
txTask (RBUS {rs, msgOffset, msgSize, msgTTL, msgQueue, msgBuff, txBuff, txLock}) = do
    locked <- getValue txLock
    when (iNot locked) $ do
        peek msgQueue $ \i -> do
            let ix = toIx i
            ttl <- getItem msgTTL ix
            ifte_ (ttl >? 0)
                  (do size <- getItem msgSize ix
                      for (toIx size) $ \dx -> do
                          sx <- getValue msgOffset
                          v <- getItem msgBuff $ toIx sx
                          setItem txBuff dx v
                          setValue msgOffset $ sx + 1
                      setItem msgTTL ix $ ttl - 1
                      let buff = toCArray $ getBuffer txBuff
                      RS.transmit rs buff size
                      setValue txLock true
                  )
                  (remove msgQueue)



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
          (ping r)
          (discovery r)



ping :: RBUS -> Ivory (ProcEffects s ()) ()
ping = toRS transmitPing

discovery :: RBUS -> Ivory (ProcEffects s ()) ()
discovery = toRS transmitDiscovery

confirm :: RBUS -> Ivory (ProcEffects s ()) ()
confirm = toRS transmitConfirm



toRS :: (Slave 255 -> (Uint8 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s ()) ())
     -> RBUS
     -> Ivory (ProcEffects s ()) ()
toRS transmit (RBUS {rs, protocol, txBuff, txLock}) = do
    locked <- getValue txLock
    when (iNot locked) $ do
        size <- local $ ival 0
        let go :: Uint8 -> Ivory eff ()
            go v = do
                i <- deref size
                let ix = toIx i
                setItem txBuff ix $ safeCast v
                store size $ i + 1
        transmit protocol go
        let array = toCArray $ getBuffer txBuff
        RS.transmit rs array =<< deref size
        setValue txLock true



instance Transport RBUS where
    transmit (RBUS {protocol, msgQueue, msgBuff, msgSize, msgTTL, msgOffset}) buff = do
        push msgQueue $ \i -> do
            size <- local $ ival 0
            offset <- getValue msgOffset
            let go :: Uint8 -> Ivory eff ()
                go v = do
                    i <- deref size
                    let ix = toIx $ offset + i
                    setItem msgBuff ix $ safeCast v
                    store size $ i + 1
            transmitMessage buff protocol go
            let ix = toIx i
            setItem msgSize ix =<< deref size
            setItem msgTTL ix messageTTL
