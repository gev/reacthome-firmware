{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Transport.UART.RBUS    where

import           Control.Monad.Reader     (MonadReader, asks)
import           Control.Monad.State      (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Controller
import           Core.Dispatcher
import qualified Core.Domain              as D
import           Core.Handler
import           Core.Task
import           Core.Transport
import           Core.Version
import           Data.Buffer
import           Data.Queue
import           Data.Value
import           GHC.TypeNats
import qualified Interface.MCU            as I
import           Interface.UART
import           Ivory.Language
import qualified Protocol.UART.RBUS       as U
import           Transport.UART.RBUS.Data
import           Transport.UART.RBUS.Rx
import           Transport.UART.RBUS.Tx



rbusTop :: ( MonadState Context m, MonadReader (D.Domain p c) m
           ,  UART (u 32 300), Controller c
           )
        => (p -> m (u 32 300)) -> m (RBUS 32 300)
rbusTop uart' = rbus uart' 115_200


rbusTopGD :: ( MonadState Context m, MonadReader (D.Domain p c) m
             , UART (u 32 300), Controller c
             )
          => (p -> m (u 32 300)) -> m (RBUS 32 512)
rbusTopGD uart' = rbus uart' 115_200

rbusHub :: ( MonadState Context m
           , MonadReader (D.Domain p c) m
           , UART (u 300 300), Controller c
           )
        => (p -> m (u 300 300)) -> m (RBUS 32 1200)
rbusHub uart' = rbus uart' 1_000_000


rbusEcho :: ( MonadState Context m
           , MonadReader (D.Domain p c) m
           , UART (u 32 300), Controller c
           )
        => (p -> m (u 32 300)) -> m (RBUS 32 300)
rbusEcho uart' = rbus uart' 1_000_000



rbus :: ( MonadState Context m, MonadReader (D.Domain p c) m
        , UART (u rn tn), Controller c
        , KnownNat q, KnownNat l, KnownNat rn, KnownNat tn
        )
     => (p -> m (u rn tn)) -> Uint32 -> m (RBUS q l)
rbus uart' speed = do
    mcu            <- asks D.mcu
    implementation <- asks D.implementation
    uart           <- uart' $ I.peripherals mcu
    let name        = "transport_uart_rbus"
    {--
        TODO: move dispatcher outside
    --}
    rbus <- mkRbus name uart speed $ makeDispatcher implementation

    addTask $ delay 1_000 (name <> "_discovery") $ discoveryTask rbus

    pure rbus



mkRbus :: ( MonadState Context m, MonadReader (D.Domain p c) m
          , UART (u rn tn)
          , KnownNat q, KnownNat l, KnownNat rn, KnownNat tn
          )
       => String
       -> u rn tn
       -> Uint32
       -> (forall s. Buffer 255 Uint8 -> Uint8 -> Ivory (ProcEffects s ()) ())
       -> m (RBUS q l)
mkRbus name uart speed onMessage = do
    mcu           <- asks D.mcu
    model         <- asks D.model
    version       <- asks D.version
    let mac        = I.mac mcu
    let clock      = I.systemClock mcu
    msgQueue      <- queue  (name <> "_msg"         ) =<< messages name
    msgBuff       <- buffer (name <> "_msg"         )
    msgIndex      <- value  (name <> "_msg_index"   ) 0
    discoveryBuff <- buffer (name <> "_discovery"   )
    txLock        <- value  (name <> "_tx_lock"     ) false
    rxTimestamp   <- value  (name <> "_timestamp_rx") 0

    protocol <- U.rbus name onMessage

    let rbus = RBUS { name, speed
                    , model, version, mac
                    , clock, uart, protocol
                    , msgQueue, msgBuff, msgIndex
                    , txLock
                    , discoveryBuff
                    , rxTimestamp
                    }

    addHandler $ HandleUART uart (rxHandle rbus) (txHandle rbus) Nothing (errorHandle rbus)

    addInit name $ initialize rbus

    addTask $ yeld (name <> "_rx"   ) $ rxTask    rbus
    addTask $ yeld (name <> "_tx"   ) $ txTask    rbus
    addTask $ yeld (name <> "_reset") $ resetTask rbus

    pure rbus



initialize RBUS{..} = do
    store (discoveryBuff ! 0) actionDiscovery
    arrayMap $ \ix -> do
        let jx = toIx $ 1 + fromIx ix
        store (discoveryBuff ! jx) =<< deref (mac ! ix)
    store (discoveryBuff ! 7) =<< deref model
    store (discoveryBuff ! 8) =<< deref (version ~> major)
    store (discoveryBuff ! 9) =<< deref (version ~> minor)
    configUART uart speed WL_8b SB_1b None



instance (KnownNat q, KnownNat l) => Transport (RBUS q l) where
    transmitBuffer = toQueue


instance (KnownNat q, KnownNat l) => LazyTransport (RBUS q l) where
    lazyTransmit = toQueue'
