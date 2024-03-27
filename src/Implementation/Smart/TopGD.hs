{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Implementation.Smart.TopGD where

import           Control.Monad.Reader    (MonadReader, asks)
import           Control.Monad.State     (MonadState)
import           Core.Actions
import           Core.Context
import           Core.Controller
import qualified Core.Domain             as D
import           Core.Handler            (addHandler)
import           Core.Task
import           Core.Transport
import           Data.Buffer
import           Data.Value
import           Endpoint.DInputs        as E (DInputs)
import           Feature.DInputs         as DI (DInputs (getDInputs),
                                                forceSyncDInputs)
import           Feature.RS485.RBUS.Data (RBUS (shouldConfirm))
import           Feature.Sht21           (SHT21)
import           Feature.Smart.Top.LEDs  (LEDs, mkLeds, onFindMe, onInitColors,
                                          onSetColor, render, update)
import           GHC.TypeNats
import           Interface.Display       (Display, Render (Render))
import           Interface.MCU           (peripherals)
import           Ivory.Language
import           Ivory.Stdlib



data Top = Top
    { dinputs    :: DI.DInputs
    , leds       :: LEDs   8 2
    , sht21      :: SHT21
    , shouldInit :: Value    IBool
    , initBuff   :: Values 1 Uint8
    , transmit   :: forall n. KnownNat n
                 => Buffer n Uint8 -> forall s t. Ivory (ProcEffects s t) ()
    }



topGD :: (MonadState Context m , MonadReader (D.Domain p c) m, Transport t, Display d)
      => m t -> (Bool -> t -> m DI.DInputs) -> (t -> m SHT21) -> (p -> m d) -> m Top
topGD transport' dinputs' sht21' display' = do
    transport          <- transport'
    shouldInit         <- asks D.shouldInit
    mcu                <- asks D.mcu
    display            <- display' $ peripherals mcu
    dinputs            <- dinputs' False transport
    let runFrameBuffer  = runValues "top_frame_buffer" $ replicate 18 0
    leds               <- mkLeds runFrameBuffer (getDInputs dinputs) [0, 5, 1, 4, 2, 3] transport
    sht21              <- sht21' transport
    initBuff           <- values "top_init_buffer" [actionInitialize]
    let top             = Top { dinputs, leds, sht21
                              , initBuff, shouldInit
                              , transmit = transmitBuffer transport
                              }
    runFrameBuffer addArea

    addHandler $ Render display 30 runFrameBuffer $ do
        update leds
        render leds

    addTask $ delay 1_000 "top_init" $ initTop top

    pure top



initTop :: Top -> Ivory (ProcEffects s t) ()
initTop Top{..} = do
    shouldInit' <- deref shouldInit
    when shouldInit' $ transmit initBuff



instance Controller Top where

    handle Top{..} buff size = do
        action <- deref $ buff ! 0
        cond_ [ action ==? actionRGB        ==> onSetColor       leds buff size
              , action ==? actionInitialize ==> onInitColors     leds buff size
              , action ==? actionFindMe     ==> onFindMe         leds buff size
              , action ==? actionGetState   ==> forceSyncDInputs dinputs
              ]
