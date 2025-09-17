{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Device.GD32F3x0.Touch where

import           Control.Monad                  (replicateM_)
import           Control.Monad.State
import           Core.Context
import           Core.Handler
import           Core.Task
import           Data.ByteString                (index)
import           Data.Value
import           Device.GD32F3x0.EXTI
import           Device.GD32F3x0.Timer
import           Interface.Counter              (Counter (readCounter))
import qualified Interface.Timer                as I
import qualified Interface.Touch                as I
import           Ivory.Language                 hiding (setBit)
import           Ivory.Language.Uint            (Uint32 (Uint32))
import           Ivory.Stdlib                   (ifte, when)
import           Ivory.Stdlib.Control
import           Ivory.Support                  (symbol)
import           Support.CMSIS.CoreCMFunc       (disableIRQ, enableIRQ)
import           Support.Device.GD32F3x0.EXTI
import           Support.Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.SYSCFG




data Touch = Touch { port          :: GPIO_PERIPH
                   , pin           :: GPIO_PIN
                   , timer         :: Timer
                   , thresholdLow  :: IFloat
                   , thresholdHigh :: IFloat
                   , moments       :: Values 5 IFloat
                   , prev          :: Value IFloat
                   , stateTouch    :: Value IBool
                   , debugVal      :: Value IFloat
                   }


mkTouch :: (MonadState Context m)
        => GPIO_PERIPH -> GPIO_PIN -> RCU_PERIPH -> Uint32 -> IFloat -> IFloat -> m Touch
mkTouch port pin rcuPin index thresholdLow thresholdHigh = do
    timer      <- cfg_timer_14 84_000_000 0xffff_ffff

    let name    = symbol port <> "_" <> symbol pin

    moments    <- values_ ("touch_moments" <> name)
    prev       <- value   ("touch_prev" <> name) 0
    stateTouch <- value   ("touch_state_touch" <> name) false
    debugVal   <- value   ("debug_val" <> name) 0

    addInit name $ do
        enablePeriphClock rcuPin
        modePort port pin gpio_mode_output
        resetBit port pin

    let touch = Touch { port
                      , pin
                      , timer
                      , thresholdLow
                      , thresholdHigh
                      , moments
                      , prev
                      , stateTouch
                      , debugVal
                      }

    addTask $ delayPhase 6 index ("touch_run" <> name) $ runMeasurement touch

    pure touch

modePort :: GPIO_PERIPH -> GPIO_PIN -> GPIO_MODE  -> Ivory eff ()
modePort gpio pin mode = do
    setOutputOptions gpio gpio_otype_pp gpio_ospeed_50mhz pin
    setMode gpio mode gpio_pupd_none pin

instance I.Touch Touch where
    run = runMeasurement
    getDebug Touch{..} = deref debugVal
    getState Touch{..} = deref stateTouch


runMeasurement :: Touch -> Ivory (ProcEffects s ()) ()
runMeasurement t@Touch{..} = do
    disableIRQ
    min <- local $ ival 0xffff_ffff
    max <- local $ ival 0
    sum <- local $ ival 0
    store sum 0
    arrayMap $ \ix -> do
        modePort port pin gpio_mode_output
        resetBit port pin
        modePort port pin gpio_mode_input
        I.resetCounter timer
        forever $ do
            isMeasured <- getInputBit port pin
            when isMeasured breakOut
        moment <- safeCast <$> I.getCounter timer
        min' <- deref min
        when (moment <? min') $ store min moment
        max' <- deref max
        when (moment >? max') $ store max moment
        store (moments ! ix) moment
        sum' <- deref sum
        store sum $ sum' + moment
    enableIRQ

    min'' <- deref min
    max'' <- deref max
    sum'' <- deref sum
    let m = (sum'' - min'' - max'') / (arrayLen moments - 2)

    prev' <- deref prev
    store prev m

    store debugVal $ m - prev'


average :: IFloat -> IFloat -> IFloat -> IFloat
average alpha a b = do
    a * (1 - alpha) + b * alpha
