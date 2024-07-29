{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Implementation.Smart.TopA4T where

import           Control.Monad.Reader          (MonadReader, asks)
import           Control.Monad.State           (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Controller
import qualified Core.Domain                   as D
import           Core.Handler
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Display.Canvas1D         (Canvas1DSize)
import           Data.Value
import           Endpoint.DInputs              as E (DInputs)
import           Feature.DInputs               as DI (DInputs (getDInputs),
                                                  forceSyncDInputs)
import           Feature.RS485.RBUS.Data       (RBUS (shouldConfirm))
import           Feature.Sht21                 (SHT21)
import           Feature.Smart.Top.Buttons
import           Feature.Smart.Top.LEDs        (LEDs, mkLeds, onBlink, onDim, onDo,
                                                onImage, onPalette, onSetColor,
                                                render, sendLEDs, updateLeds)

import           Feature.Smart.Top.Vibro      (Vibro, onInitVibro, onVibro,
                                               sendVibro, vibro)
import           Feature.Smart.Top.PowerTouch (PowerTouch)
import           GHC.TypeNats
import           Interface.Display            (Display, Render (Render))
import           Interface.Flash
import           Interface.MCU                (peripherals)
import           Ivory.Language
import           Ivory.Stdlib
import Data.Matrix



data Top n = Top
    { dinputs :: DI.DInputs n
    , leds    :: LEDs       4 5
    , buttons :: Buttons    n 4 5
    , vibro   :: Vibro      n
    , sht21   :: SHT21
    }



topA4T :: ( MonadState Context m
         , MonadReader (D.Domain p c) m
         , Display d, Handler (Render (Canvas1DSize 5)) d
         , LazyTransport t
         , Flash f
         , KnownNat n
         )
      => m t 
      -> (Bool -> t -> m (DI.DInputs n)) 
      -> (E.DInputs n -> t -> f-> m (Vibro n)) 
      -> m PowerTouch 
      -> (t -> m SHT21) 
      -> (p -> m d) 
      -> (p -> f) 
      -> m (Top n)
topA4T transport' dinputs' vibro' touch' sht21' display' etc' = do
    transport     <- transport'
    shouldInit    <- asks D.shouldInit
    mcu           <- asks D.mcu
    display       <- display' $ peripherals mcu
    let etc        = etc' $ peripherals mcu
    dinputs       <- dinputs' True transport
    vibro         <- vibro' (DI.getDInputs dinputs) transport etc
    touch'
    frameBuffer   <- values' "top_frame_buffer" 0
    leds          <- mkLeds frameBuffer [0, 4, 1, 2, 3] transport etc
    ledsPerButton <- values "led_on_buttons_n" [3, 3, 3, 3]
    ledsOfButton  <- matrix "led_on_buttons"   [[0,1,2,0], [0,2,3,0], [1,2,4,0], [2,3,4,0]]
    buttons       <- mkButtons leds (DI.getDInputs dinputs) ledsPerButton ledsOfButton transport
    sht21         <- sht21' transport
    let top       = Top { dinputs, leds, vibro, buttons, sht21 }

    addHandler $ Render display 30 frameBuffer $ do
        updateLeds leds
        updateButtons buttons
        render leds

    pure top



onGetState :: KnownNat n => Top n -> Ivory (ProcEffects s t) ()
onGetState Top{..} = do
    forceSyncDInputs dinputs
    sendVibro vibro
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
              , action ==? actionVibro      ==> onVibro          vibro   buff size
              , action ==? actionFindMe     ==> onFindMe         buttons buff size
              , action ==? actionGetState   ==> onGetState       t
              ]
