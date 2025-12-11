{-# HLINT ignore "Use for_" #-}
module Device.GD32F3x0.Touch where

import Control.Monad.State
import Core.Context
import Core.Task
import Data.Value
import Device.GD32F3x0.Timer
import Device.GD32F3x0.Timer qualified as T
import Interface.Touch qualified as I
import Ivory.Language hiding (setBit)
import Ivory.Stdlib.Control
import Ivory.Support (symbol)
import Support.Device.GD32F3x0.GPIO
import Support.Device.GD32F3x0.RCU
import Support.Device.GD32F3x0.Timer
import Data.Concurrent.Atomically

data Touch = Touch
    { port :: GPIO_PERIPH
    , pin :: GPIO_PIN
    , afPin :: GPIO_AF
    , timer :: Timer
    , timerChannel :: TIMER_CHANNEL
    , timerChannelFlag :: TIMER_FLAG
    , material :: I.Material
    , timestamp :: Value Uint16
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
    GPIO_AF ->
    (Uint32 -> Uint32 -> m Timer) ->
    TIMER_CHANNEL ->
    TIMER_FLAG ->
    I.Material ->
    m Touch
mkTouch port pin rcuPin afPin timer' timerChannel timerChannelFlag material = do
    timer <- timer' 84_000_000 0xffff

    let name = symbol port <> "_" <> symbol pin

    timestamp <- value ("touch_timestamp" <> name) 0
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
        pinToGround port pin
        let t = T.timer timer
        configTimerInputCapture t timerChannel =<< local (istruct timerIcDefaultParam)
        enableTimer t

    let touch =
            Touch
                { port
                , pin
                , afPin
                , timer
                , timerChannel
                , timerChannelFlag
                , material
                , timestamp
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
    addTask $ yeld ("touch_run" <> name) $ processingMeasurement touch

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

pinToGround :: GPIO_PERIPH -> GPIO_PIN -> Ivory eff ()
pinToGround gpio pin = do
    setOutputOptions gpio gpio_otype_pp gpio_ospeed_50mhz pin
    setMode gpio gpio_mode_output gpio_pupd_none pin
    resetBit gpio pin

pinToAF :: GPIO_PERIPH -> GPIO_PIN -> GPIO_AF -> Ivory eff ()
pinToAF gpio pin af = do
    setMode gpio gpio_mode_af gpio_pupd_none pin
    setOutputOptions gpio gpio_otype_pp gpio_ospeed_50mhz pin
    setAF gpio af pin

instance I.Touch Touch where
    getDebug Touch{..} = deref debugVal
    getState Touch{..} = deref stateTouch

startMeasurement :: Touch -> Ivory eff ()
startMeasurement Touch{..} = do
    pinToGround port pin
    let t = T.timer timer
    clearTimerFlag t timerChannelFlag
    atomically do 
        pinToAF port pin afPin
        store timestamp . castDefault =<< readCounter t

processingMeasurement :: Touch -> Ivory eff ()
processingMeasurement touch@Touch{..} = do
    let t = T.timer timer
    isReady <- getTimerFlag t timerChannelFlag
    when isReady do
        t0 <- deref timestamp
        t1 <- castDefault <$> readTimerChannelRegister t timerChannel

        startMeasurement touch

        let moment = safeCast $ t1 - t0

        avg' <- deref avg
        avg0' <- deref avg0
        avg1' <- deref avg1

        let diff = abs $ moment - avg'

        -- store debugVal moment

        start' <- deref start

        ifte_
            start'
            do
                when (moment >? 0 .&& avg' >? 0 .&& diff <? I.maxDiff material) do
                    var' <- deref var
                    store var $ average 0.01 var' $ diff * diff
                    var'' <- (/ avg') <$> deref var

                    store debugVal $ 100 * var''

                    ifte_
                        (var'' >? I.thresholdUp material .&& moment >? avg')
                        do
                            counter' <- deref counter
                            store counter $ counter' + 1
                            counter'' <- deref counter
                            when (counter'' ==? 100) do
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

aluminum =
    I.Material
        { maxDiff = 1800
        , thresholdUp = 300
        , thresholdDown = 50
        }

glass =
    I.Material
        { maxDiff = 80
        , thresholdUp = 0.5
        , thresholdDown = 0.2
        }
