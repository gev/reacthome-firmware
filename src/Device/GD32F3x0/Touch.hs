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

import           Control.Monad.State
import           Core.Context
import           Core.Handler
import           Core.Task
import           Data.Value
import           Device.GD32F3x0.EXTI
import           Device.GD32F3x0.Timer
import qualified Interface.Counter              as I
import qualified Interface.Touch                as I
import           Ivory.Language                 hiding (setBit)
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
                   , timeAvg       :: Value IFloat
                   , time          :: Value IFloat
                   , delta         :: Value IFloat
                   , stateTouch    :: Value IBool
                   , debugVal      :: Value IFloat
                   }


mkTouch :: (MonadState Context m)
        => GPIO_PERIPH -> GPIO_PIN -> RCU_PERIPH -> IFloat -> IFloat -> m Touch
mkTouch port pin rcuPin thresholdLow thresholdHigh = do

    timer <- cfg_timer_14 84_000_000 0xffff_ffff

    let name = symbol port <> "_" <> symbol pin

    timeAvg           <- value ("touch_time_avg" <> name) 0
    time              <- value ("touch_time" <> name) 0
    delta             <- value ("touch_delta" <> name) 0
    stateTouch        <- value ("touch_state_touch" <> name) false
    debugVal          <- value ("debug_val" <> name) 0

    addInit name $ do

        enablePeriphClock rcuPin
        modePort port pin gpio_mode_output
        resetBit port pin

    let touch = Touch { port
                      , pin
                      , timer
                      , thresholdLow
                      , thresholdHigh
                      , timeAvg
                      , time
                      , delta
                      , stateTouch
                      , debugVal
                      }

    -- addTask $ delay 5_000 ("reset_avg" <> name) $ resetAvg touch

    pure touch

resetAvg :: Touch -> Ivory eff ()
resetAvg Touch{..} = do
    state <- deref stateTouch
    when (iNot state) $
        store timeAvg 0xffff

modePort :: GPIO_PERIPH -> GPIO_PIN -> GPIO_MODE  -> Ivory eff ()
modePort gpio pin mode = do
    setOutputOptions gpio gpio_otype_pp gpio_ospeed_50mhz pin
    setMode gpio mode gpio_pupd_none pin

instance I.Touch Touch where
    run = runMeasurement
    getDebug Touch{..} = deref debugVal
    getState Touch{..} = deref stateTouch
    reset Touch{..} = do
                modePort port pin gpio_mode_output
                resetBit port pin
    pause Touch{..} = do
                t1 <- I.readCounter timer
                forever $ do
                    t2 <- I.readCounter timer
                    when (t2 - t1 >? 420) breakOut


runMeasurement :: Touch -> Ivory (ProcEffects s ()) ()
runMeasurement t@Touch{..} = do
    modePort port pin gpio_mode_input

    disableIRQ
    t1 <- I.readCounter timer
    forever $ do
        isMeasured <- getInputBit port pin
        when isMeasured breakOut
    t2 <- I.readCounter timer
    enableIRQ

    modePort port pin gpio_mode_output
    resetBit port pin

    -- forever $ do
    --     t3 <- I.readCounter timer
    --     when (t3 - t2 >? 420) breakOut

    let t = safeCast $ t2 - t1

    time' <- deref time
    store time    $ average 0.01 time' t

    avg'  <- deref timeAvg
    store timeAvg $ average 0.001 avg' t

    time'' <- deref time
    avg'' <- deref timeAvg
    let t = time'' - avg''
    delta' <- deref delta
    -- state <- deref stateTouch
    cond_ [ t >? thresholdHigh ==> do
                store stateTouch true
                store delta $ (- t) / 2
                -- when (iNot state) $ store debugVal t
          , t <? delta'  ==> do
                store stateTouch false
                -- when state $ store debugVal t
          ]

    -- delta' <- deref delta
    -- cond_ [ t >? thresholdHigh ==> store delta (average 0.02 delta' 1)
    --       , t <? thresholdLow  ==> store delta (average 0.02 delta' 0)
    --       ]

    -- delta'' <- deref delta
    -- cond_ [ delta'' >? 0.8 ==> store stateTouch true
    --       , delta'' <? 0.2 ==> store stateTouch false
    --       ]


    store debugVal t -- $ delta'' * 100




average :: IFloat -> IFloat -> IFloat -> IFloat
average alpha a b = do
    a * (1 - alpha) + b * alpha
