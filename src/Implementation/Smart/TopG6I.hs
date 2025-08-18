{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}

module Implementation.Smart.TopG6I where

import           Control.Monad.Reader         (MonadReader, asks)
import           Control.Monad.State          (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Controller
import qualified Core.Domain                  as D
import           Core.Handler
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Display.Canvas1D        (Canvas1DSize)
import           Data.Value
import           Endpoint.DInputs             as E (DInputs)
import           Feature.RS485.RBUS.Data      (RBUS (shouldConfirm))
import           Feature.Sht21                (SHT21)
import           Feature.Smart.Top.Buttons
import           Feature.Smart.Top.LEDs       (LEDs, mkLeds, onBlink, onDim,
                                               onDo, onImage, onPalette,
                                               onSetColor, render, sendLEDs,
                                               updateLeds)

import           Data.Matrix
import           Feature.Smart.Top.PowerTouch (PowerTouch)
import           Feature.Smart.Top.Vibro      (Vibro, onInitVibro, onVibro,
                                               sendVibro, vibro)
import           GHC.TypeNats
import           Interface.Display            (Display, Render (Render))
import           Interface.Flash
import           Interface.MCU                (peripherals)
import           Ivory.Language
import           Ivory.Stdlib
import qualified Feature.Touches as DT



data Top n = Top
    { touches :: DT.Touches n
    -- , leds    :: LEDs       4 12
    -- , buttons :: Buttons    n 4 12
    -- , vibro   :: Vibro      n
    -- , sht21   :: SHT21
    }



topG6I :: ( MonadState Context m
         , MonadReader (D.Domain p c) m
         , LazyTransport t
         , KnownNat n
         )
      => m t
      -> (t -> m (DT.Touches n))
      -> m (Top n)
topG6I transport' touches' = do
    transport      <- transport'
    -- shouldInit     <- asks D.shouldInit
    mcu            <- asks D.mcu
    touches        <- touches' transport
    -- vibro          <- vibro' (DI.getDInputs dinputs) transport etc
    -- buttons        <- mkButtons leds (DI.getDInputs dinputs) ledsPerButton ledsOfButton transport
    -- let top         = Top { touches, leds, vibro, buttons, sht21 }
    let top         = Top { touches }

    -- addHandler $ Render display 30 frameBuffer $ do
    --     updateLeds    leds
    --     updateButtons buttons
    --     render        leds
    --     pure          true

    pure top



-- onGetState :: KnownNat n => Top n -> Ivory (ProcEffects s t) ()
-- onGetState Top{..} = do
--     forceSyncDInputs dinputs
--     sendVibro vibro
--     sendLEDs leds



instance KnownNat n => Controller (Top n) where

    handle t@Top{..} buff size = do
        action <- deref $ buff ! 0
        cond_ []
--               , action ==? actionDim        ==> onDim            leds    buff size
--               , action ==? actionRGB        ==> onSetColor       leds    buff size
--               , action ==? actionImage      ==> onImage          leds    buff size
--               , action ==? actionBlink      ==> onBlink          leds    buff size
--               , action ==? actionPalette    ==> onPalette        leds    buff size
--               , action ==? actionVibro      ==> onVibro          vibro   buff size
--               , action ==? actionFindMe     ==> onFindMe         buttons buff size
            --   , 
--               ]
