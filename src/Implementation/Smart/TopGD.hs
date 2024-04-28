{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Implementation.Smart.TopGD where

import           Control.Monad.Reader         (MonadReader, asks)
import           Control.Monad.State          (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Controller
import qualified Core.Domain                  as D
import           Core.Handler                 (Handler, addHandler)
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Value
import           Device.GD32F3x0.ADC          (ADC (buff))
import           Endpoint.DInputs             as E (DInputs)
import           Feature.DInputs              as DI (DInputs (getDInputs),
                                                     forceSyncDInputs)
import           Feature.RS485.RBUS.Data      (RBUS (shouldConfirm))
import           Feature.Sht21                (SHT21)
import           Feature.Smart.Top.Buttons
import           Feature.Smart.Top.LEDs       (LEDs, mkLeds, onBlink, onDim,
                                               onDo, onImage, onInitColors,
                                               onSetColor, render, updateLeds)
import           Feature.Smart.Top.PowerTouch (PowerTouch)
import           Feature.Smart.Top.Vibro      (Vibro, onInitVibro, onVibro,
                                               vibro)
import           GHC.TypeNats
import           Interface.Display            (Display, Render (Render))
import           Interface.MCU                (peripherals)
import           Ivory.Language
import           Ivory.Stdlib



data Top = Top
    { dinputs    :: DI.DInputs 4
    , leds       :: LEDs      64
    , buttons    :: Buttons    4 64
    , vibro      :: Vibro      4
    , sht21      :: SHT21
    , shouldInit :: Value    IBool
    , initBuff   :: Values 1 Uint8
    , transmit   :: forall n. KnownNat n
                 => Buffer n Uint8 -> forall s t. Ivory (ProcEffects s t) ()
    }


{- The LEDs configuration:

      6 7                                        0 1

    8     17    22 23 30    35 36 43    49 50 57
    9     16    21    29    34    42    48    56     58 63
   10  12 15    20 24 28    33 37 41    47 51 55     59 62
          14    19    27    32    40    46    54     60 61
   11     13    18 25 26    31 38 39 44 45 52 53

      5 4                                        3 2
-}

topGD :: ( MonadState Context m
         , MonadReader (D.Domain p c) m
         , Transport t, LazyTransport t
         , Display d
         , Handler (Render 192) d
         )
      => m t
      -> (Bool -> t -> m (DI.DInputs 4))
      -> (E.DInputs 4 -> t -> m (Vibro 4))
      -> m PowerTouch
      -> (t -> m SHT21)
      -> (p -> m d)
      -> m Top
topGD transport' dinputs' vibro' touch' sht21' display' = do
    transport    <- transport'
    shouldInit   <- asks D.shouldInit
    mcu          <- asks D.mcu
    display      <- display' $ peripherals mcu
    dinputs      <- dinputs' True transport

    frameBuffer  <- values' "top_frame_buffer" 0

    vibro        <- vibro' (DI.getDInputs dinputs) transport

    touch'

    leds         <- mkLeds frameBuffer [  6,  7,      0,  1
                                       ,  5,  4,      3,  2

                                       ,  8
                                       ,  9
                                       , 10

                                       , 11

                                       , 58, 63
                                       , 59, 62
                                       , 60, 61

                                       ,     17,     22, 23, 30,     35, 36, 43,     49, 50, 57
                                       ,     16,     21,     29,     34,     42,     48,     56
                                       , 12, 15,     20, 24, 28,     33, 37, 41,     47, 51, 55
                                       ,     14,     19,     27,     32,     40,     46,     54
                                       ,     13,     18, 25, 26,     31, 38, 39, 44, 45, 52, 53
                                       ] transport

    buttons      <- mkButtons leds (DI.getDInputs dinputs) 2 transport

    sht21        <- sht21' transport

    initBuff     <- values "top_init_buffer" [actionInitialize]

    let top       = Top { dinputs, leds, vibro, buttons, sht21
                        , initBuff, shouldInit
                        , transmit = transmitBuffer transport
                        }

    addHandler $ Render display 30 frameBuffer $ do
        updateLeds leds
        updateButtons buttons
        render leds

    addTask $ delay 1_000 "top_init" $ initTop top

    pure top



initTop :: Top -> Ivory (ProcEffects s t) ()
initTop Top{..} = do
    shouldInit' <- deref shouldInit
    when shouldInit' $ transmit initBuff



onInit :: KnownNat n => Top -> Buffer n Uint8 -> Uint8 -> Ivory (ProcEffects s t) ()
onInit Top{..} buff size = do
    colors <- onInitColors leds  buff size
    vibro  <- onInitVibro  vibro buff size
    when (colors .&& vibro) $
        store shouldInit false


instance Controller Top where

    handle t@Top{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionDo         ==> onDo             leds    buff size
              , action ==? actionDim        ==> onDim            leds    buff size
              , action ==? actionRGB        ==> onSetColor       leds    buff size
              , action ==? actionImage      ==> onImage          leds    buff size
              , action ==? actionBlink      ==> onBlink          leds    buff size
              , action ==? actionVibro      ==> onVibro          vibro   buff size
              , action ==? actionInitialize ==> onInit           t       buff size
              , action ==? actionFindMe     ==> onFindMe         buttons buff size
              , action ==? actionGetState   ==> forceSyncDInputs dinputs
              ]
