{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Feature.Smart.Top where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState (state))
import Core.Actions
import Core.Context
import qualified Core.Domain as D
import Core.Task
import Core.Transport
import Data.Buffer
import Data.Value
import Feature.Dimmers (forceSync)
import GHC.TypeNats
import Interface.GPIO.Input
import Interface.GPIO.Port
import Interface.MCU
import qualified Interface.UART as I
import Ivory.Language
import Ivory.Stdlib
import Transport.UART.RBUS
import Transport.UART.RBUS.Data

data Top where
    Top ::
        (Input i, LazyTransport t) =>
        { pin :: i
        , isDetected :: Value IBool
        , transportUp :: t
        , transportDown :: RBUS 32 512
        } ->
        Top

top ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , I.UART (u 32 300)
    , Input i
    , Pull p d
    , LazyTransport t
    ) =>
    (p -> m (u 32 300)) ->
    (p -> d -> m i) ->
    t ->
    m Top
top uart' pin' transportUp = do
    mcu <- asks D.mcu
    let peripherals' = peripherals mcu
    uart <- uart' peripherals'
    pin <- pin' peripherals' $ pullDown peripherals'

    isDetected <- value "top_is_detected" false

    let onMessage' ::
            (KnownNat n) =>
            Buffer n Uint8 ->
            Uint8 ->
            Ivory (ProcEffects s t) ()
        onMessage' buff size = do
            action <- deref $ buff ! 0
            ifte_
                (action ==? actionFindMe)
                ( when (size ==? 2) $
                    lazyTransmit transportUp 2 $ \transmit -> do
                        transmit actionFindMe
                        transmit =<< deref (buff ! 1)
                )
                ( lazyTransmit transportUp (size + 1) $ \transmit -> do
                    transmit actionSmartTop
                    for (toIx size) $ \ix ->
                        transmit =<< deref (buff ! ix)
                )

    transportDown <- mkRbus "transport_uart_rbus" uart 115_200 onMessage'

    let top = Top{pin, isDetected, transportUp, transportDown}

    addTask $ delay 100 "top_detect" $ detect top

    pure top

detect :: Top -> Ivory (ProcEffects s t) ()
detect Top{..} = do
    state <- get pin
    isDetected' <- deref isDetected
    when (isDetected' /=? state) $ do
        lazyTransmit transportUp 2 $ \transmit -> do
            transmit actionSmartTopDetect
            transmit $ safeCast state
        store isDetected state

onMessage ::
    (KnownNat n) =>
    Top ->
    Buffer n Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onMessage Top{..} buff size = do
    let size' = size - 1
    lazyTransmit transportDown size' $ \transmit ->
        upTo 1 (toIx size') $ \ix ->
            transmit =<< deref (buff ! ix)

onFindMe ::
    (KnownNat n) =>
    Top ->
    Buffer n Uint8 ->
    Uint8 ->
    Ivory (ProcEffects s t) ()
onFindMe Top{..} buff size =
    when (size ==? 2) $
        lazyTransmit transportDown 2 $ \transmit -> do
            transmit actionFindMe
            transmit =<< deref (buff ! 1)

forceSyncTop :: Top -> Ivory (ProcEffects s t) ()
forceSyncTop Top{..} =
    lazyTransmit transportDown 1 $ \transmit ->
        transmit actionGetState
