{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

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
import           Data.Value
import           Endpoint.DInputs          as E (DInputs)
import           Feature.DInputs           as DI (DInputs (getDInputs),
                                                  forceSyncDInputs)
import           Feature.RS485.RBUS.Data   (RBUS (shouldConfirm))
import           Feature.Sht21             (SHT21)
import           Feature.Smart.Top.Buttons
import           Feature.Smart.Top.LEDs    (LEDs, mkLeds, onBlink, onDim, onDo,
                                            onImage, onInitColors, onSetColor,
                                            render, updateLeds)
import           GHC.TypeNats
import           Interface.Display         (Display, Render (Render))
import           Interface.MCU             (peripherals)
import           Ivory.Language
import           Ivory.Stdlib



data Top = Top
    { dinputs    :: DI.DInputs 6
    , leds       :: LEDs       6
    , buttons    :: Buttons    6 6
    , sht21      :: SHT21
    , shouldInit :: Value    IBool
    , initBuff   :: Values 1 Uint8
    , transmit   :: forall n. KnownNat n
                 => Buffer n Uint8 -> forall s t. Ivory (ProcEffects s t) ()
    }



topAP :: ( MonadState Context m
         , MonadReader (D.Domain p c) m
         , Transport t, LazyTransport t
         , Display d, Handler (Render 18) d
         )
      => m t -> (Bool -> t -> m (DI.DInputs 6)) -> (t -> m SHT21) -> (p -> m d) -> m Top
topAP transport' dinputs' sht21' display' = do
    transport   <- transport'
    shouldInit  <- asks D.shouldInit
    mcu         <- asks D.mcu
    display     <- display' $ peripherals mcu
    dinputs     <- dinputs' False transport
    frameBuffer <- values' "top_frame_buffer" 0
    leds        <- mkLeds frameBuffer [0, 5, 1, 4, 2, 3] transport
    buttons     <- mkButtons leds (DI.getDInputs dinputs) 2 transport
    sht21       <- sht21' transport
    initBuff    <- values "top_init_buffer" [actionInitialize]
    let top      = Top { dinputs, leds, buttons, sht21
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
    colors <- onInitColors leds buff size
    when colors $
        store shouldInit false


instance Controller Top where

    handle t@Top{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionDo         ==> onDo             leds    buff size
              , action ==? actionDim        ==> onDim            leds    buff size
              , action ==? actionRGB        ==> onSetColor       leds    buff size
              , action ==? actionImage      ==> onImage          leds    buff size
              , action ==? actionBlink      ==> onBlink          leds    buff size
              , action ==? actionInitialize ==> onInit           t       buff size
              , action ==? actionFindMe     ==> onFindMe         buttons buff size
              , action ==? actionGetState   ==> forceSyncDInputs dinputs
              ]
