module Implementation.Smart.TopG6Test where

import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState)
import Core.Actions
import Core.Context
import Core.Controller
import Core.Domain qualified as D
import Core.Handler
import Core.Task
import Core.Transport
import Data.Buffer
import Data.Display.Canvas1D (Canvas1DSize)
import Data.Value
import Endpoint.DInputs as E (DInputs)
import Feature.RS485.RBUS.Data (RBUS (shouldConfirm))
import Feature.Sht21 (SHT21)
import Feature.Smart.Top.Buttons
import Feature.Smart.Top.LEDs (
    LEDs,
    mkLeds,
    onBlink,
    onDim,
    onDo,
    onImage,
    onPalette,
    onSetColor,
    render,
    sendLEDs,
    updateLeds,
 )

import Data.Matrix
import Feature.Smart.Top.PowerTouch (PowerTouch)
import Feature.Smart.Top.Vibro (
    Vibro,
    onInitVibro,
    onVibro,
    sendVibro,
    vibro,
 )
import Feature.Touches qualified as FT
import GHC.TypeNats
import Interface.Display (Display, Render (Render))
import Interface.Flash
import Interface.MCU (peripherals)
import Ivory.Language
import Ivory.Stdlib

newtype Top n = Top
    { touches :: FT.Touches n
    }

topG6Test ::
    ( MonadState Context m
    , MonadReader (D.Domain p c) m
    , LazyTransport t
    , KnownNat n
    ) =>
    m t ->
    (t -> m (FT.Touches n)) ->
    m (Top n)
topG6Test transport' touches' = do
    transport <- transport'
    shouldInit <- asks D.shouldInit
    mcu <- asks D.mcu
    touches <- touches' transport
    let top = Top{touches}

    pure top

instance (KnownNat n) => Controller (Top n) where
    handle t@Top{..} buff size = do
        action <- deref $ buff ! 0
        cond_
            []
