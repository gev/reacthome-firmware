{-# HLINT ignore "Use for_" #-}
module Device.GD32F3x0.Touch where

import Control.Monad.State
import Core.Context
import Core.Task
import Data.Value
import Device.GD32F3x0.Timer
import Interface.Timer qualified as I
import Interface.Touch qualified as I
import Ivory.Language hiding (setBit)
import Ivory.Stdlib.Control
import Ivory.Support (symbol)
import Support.CMSIS.CoreCMFunc (disableIRQ, enableIRQ)
import Support.Device.GD32F3x0.GPIO
import Support.Device.GD32F3x0.RCU

data Touch = Touch
    { port :: GPIO_PERIPH
    , pin :: GPIO_PIN
    , timer :: Timer
    , material :: I.Material
    , avg :: Value IFloat
    , avg0 :: Value IFloat
    , avg1 :: Value IFloat
    , var0 :: Value IFloat
    , var1 :: Value IFloat
    , var :: Value IFloat
    , stateTouch :: Value IBool
    , start :: Value IBool
    , shouldCalibrate :: Value IBool
    , counter :: Value Uint8
    , debugVal :: Value IFloat
    }

mkTouch ::
    (MonadState Context m) =>
    GPIO_PERIPH ->
    GPIO_PIN ->
    RCU_PERIPH ->
    I.Material ->
    m Touch
mkTouch port pin rcuPin material = do
    timer <- cfg_timer_14 84_000_000 0xffff_ffff

    let name = symbol port <> "_" <> symbol pin

    avg <- value ("touch_avg" <> name) 0
    avg0 <- value ("touch_avg0" <> name) 0
    avg1 <- value ("touch_avg1" <> name) 0
    var0 <- value ("touch_var0" <> name) 0
    var1 <- value ("touch_var1" <> name) 0
    var <- value ("touch_var" <> name) 0
    stateTouch <- value ("touch_state_touch" <> name) false
    start <- value ("touch_start" <> name) false
    shouldCalibrate <- value ("touch_should_calibrate" <> name) true
    counter <- value ("touch_counter" <> name) 0
    debugVal <- value ("debug_val" <> name) 0

    addInit name do
        enablePeriphClock rcuPin
        modePort port pin gpio_mode_output
        resetBit port pin

    let touch =
            Touch
                { port
                , pin
                , timer
                , material
                , avg
                , avg0
                , avg1
                , var
                , var0
                , var1
                , stateTouch
                , start
                , shouldCalibrate
                , counter
                , debugVal
                }

    addTask $ delay 5_000 ("touch_start" <> name) $ touchStart touch
    addTask $ delay 500 ("touch_check_calibration" <> name) $ checkCalibration touch
    addTask $ yeld ("touch_run" <> name) $ runMeasurement touch

    pure touch

touchStart :: Touch -> Ivory eff ()
touchStart Touch{..} =
    store start true

checkCalibration :: Touch -> Ivory eff ()
checkCalibration Touch{..} = do
    var0' <- deref var0
    var1' <- deref var1
    shouldCalibrate' <- deref shouldCalibrate
    when (shouldCalibrate' .&& var0' >? 0 .&& var1' >? 0) do
        ifte_
            (var1' >? var0')
            do
                store shouldCalibrate false
            do
                avg1' <- deref avg1
                store avg0 avg1'
                store avg avg1'
                store var0 var1'

modePort :: GPIO_PERIPH -> GPIO_PIN -> GPIO_MODE -> Ivory eff ()
modePort gpio pin mode = do
    setOutputOptions gpio gpio_otype_pp gpio_ospeed_50mhz pin
    setMode gpio mode gpio_pupd_none pin

instance I.Touch Touch where
    getDebug Touch{..} = deref debugVal
    getState Touch{..} = deref stateTouch

runMeasurement :: Touch -> Ivory (ProcEffects s ()) ()
runMeasurement Touch{..} = do
    disableIRQ
    modePort port pin gpio_mode_input
    I.resetCounter timer
    forever do
        isMeasured <- getInputBit port pin
        moment <- I.getCounter timer
        when (isMeasured .|| moment >? I.maxMoment material) breakOut
    moment <- safeCast <$> I.getCounter timer
    enableIRQ
    modePort port pin gpio_mode_output
    resetBit port pin

    avg' <- deref avg
    avg0' <- deref avg0
    avg1' <- deref avg1

    let diff = abs $ moment - avg'

    store debugVal moment

    start' <- deref start

    ifte_
        (start' .&& moment >? 0 .&& avg' >? 0 .&& diff <? I.maxDiff material)
        do
            var' <- deref var
            store var $ average 0.01 var' $ diff * diff
            var'' <- (/ avg') <$> deref var

            -- store debugVal $ 100 * var''

            ifte_
                (var'' >? I.thresholdUp material .&& moment >? avg')
                do
                    counter' <- deref counter
                    store counter $ counter' + 1
                    counter'' <- deref counter
                    when (counter'' ==? 30) do
                        store stateTouch true
                do
                    store counter 0

            -- store debugVal . safeCast =<< deref counter

            when (var'' <? I.thresholdDown material) do
                store stateTouch false

            stateTouch' <- deref stateTouch

            ifte_
                stateTouch'
                do
                    store avg1 $ average 0.0001 avg1' moment
                    avg1'' <- deref avg1
                    var1' <- deref var1
                    let d1 = moment - avg1''
                    store var1 $ average 0.01 var1' $ d1 * d1
                do
                    store avg0 $ average 0.0001 avg0' moment
                    avg0'' <- deref avg0
                    var0' <- deref var0
                    let d0 = moment - avg0''
                    store var0 $ average 0.001 var0' $ d0 * d0

            shouldCalibrate' <- deref shouldCalibrate
            when (iNot stateTouch' .|| shouldCalibrate') do
                store avg $ average 0.0001 avg' moment
        do
            let a = average 0.001 avg' moment
            store avg a
            store avg0 a
            store avg1 a

average :: IFloat -> IFloat -> IFloat -> IFloat
average alpha a b =
    a * (1 - alpha) + b * alpha


aluminium = I.Material {
      maxMoment = 1800
    , maxDiff = 1800
    , thresholdUp = 300
    , thresholdDown = 50
}

glass = I.Material {
      maxMoment = 1800
    , maxDiff = 80
    , thresholdUp = 0.5
    , thresholdDown = 0.2
}
