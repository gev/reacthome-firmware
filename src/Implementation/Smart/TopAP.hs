{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Implementation.Smart.TopAP where

import           Control.Monad.Reader      (MonadReader, asks)
import           Control.Monad.State       (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Controller
import qualified Core.Domain               as D
import           Core.Handler
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Display.Canvas1D     (Canvas1DSize)
import           Data.Value
import           Endpoint.DInputs          as E (DInputs)
import           Feature.DInputs           as DI (DInputs (getDInputs),
                                                  forceSyncDInputs)
import           Feature.RS485.RBUS.Data   (RBUS (shouldConfirm))
import           Feature.Sht21             (SHT21)
import           Feature.Smart.Top.Buttons
import           Feature.Smart.Top.LEDs    (LEDs, mkLeds, onBlink, onDim, onDo,
                                            onImage, onPalette, onSetColor,
                                            render, sendLEDs, updateLeds)
import           GHC.TypeNats
import           Interface.Display         (Display, Render (Render))
import           Interface.Flash
import           Interface.MCU             (peripherals)
import           Ivory.Language
import           Ivory.Stdlib



data Top n = Top
    { dinputs :: DI.DInputs n
    , leds    :: LEDs       1 n
    , buttons :: Buttons    n 1 n
    , sht21   :: SHT21
    }



topAP :: ( MonadState Context m
         , MonadReader (D.Domain p c) m
         , Display d, Handler (Render (Canvas1DSize n)) d
         , LazyTransport t
         , Flash f
         , KnownNat n, KnownNat (Canvas1DSize n)
         )
      => m t -> (Bool -> t -> m (DI.DInputs n)) -> (t -> m SHT21) -> (p -> m d) -> (p -> f) -> m (Top n)
topAP transport' dinputs' sht21' display' etc' = do
    transport   <- transport'
    shouldInit  <- asks D.shouldInit
    mcu         <- asks D.mcu
    display     <- display' $ peripherals mcu
    let etc      = etc' $ peripherals mcu
    dinputs     <- dinputs' False transport
    frameBuffer <- values' "top_frame_buffer" 0
    leds        <- mkLeds frameBuffer [0, 5, 1, 4, 2, 3] transport etc
    buttons     <- mkButtons leds (DI.getDInputs dinputs) 1 transport
    sht21       <- sht21' transport
    let top      = Top { dinputs, leds, buttons, sht21 }

    addHandler $ Render display 30 frameBuffer $ do
        updateLeds leds
        updateButtons buttons
        render leds

    pure top



onGetState :: KnownNat n => Top n -> Ivory (ProcEffects s t) ()
onGetState Top{..} = do
    forceSyncDInputs dinputs
    sendLEDs leds



instance KnownNat n => Controller (Top n) where

    handle t@Top{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionDo         ==> onDo             leds    buff size
              , action ==? actionDim        ==> onDim            leds    buff size
              , action ==? actionRGB        ==> onSetColor       leds    buff size
              , action ==? actionImage      ==> onImage          leds    buff size
              , action ==? actionBlink      ==> onBlink          leds    buff size
              , action ==? actionPalette    ==> onPalette        leds    buff size
              , action ==? actionFindMe     ==> onFindMe         buttons buff size
              , action ==? actionGetState   ==> onGetState       t
              ]
