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
    , bottom :: IFloat
    , diff :: Value IFloat
    , top :: IFloat
    , avg :: Value IFloat
    , avg_ :: Value IFloat
    , sum :: Value IFloat
    , ix :: Value Uint16
    , stateTouch :: Value IBool
    , start :: Value IBool
    , debugVal :: Value IFloat
    }

mkTouch ::
    (MonadState Context m) =>
    GPIO_PERIPH ->
    GPIO_PIN ->
    RCU_PERIPH ->
    IFloat ->
    IFloat ->
    m Touch
mkTouch port pin rcuPin bottom top = do
    timer <- cfg_timer_14 84_000_000 0xffff_ffff

    let name = symbol port <> "_" <> symbol pin

    diff <- value ("touch_diff" <> name) 0
    avg <- value ("touch_avg" <> name) 0
    avg_ <- value ("touch_avg_" <> name) 0
    sum <- value ("touch_sum" <> name) 0
    ix <- value ("touch_ix" <> name) 0
    stateTouch <- value ("touch_state_touch" <> name) false
    start <- value ("touch_start" <> name) false
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
                , top
                , bottom
                , diff
                , avg
                , avg_
                , sum
                , ix
                , stateTouch
                , start
                , debugVal
                }

    -- addTask $ delay 1_000 ("touch_reset_bounds" <> name) $ resetAvg touch
    addTask $ delay 5_000 ("touch_start" <> name) $ touchStart touch
    addTask $ yeld ("touch_run" <> name) $ runMeasurement touch

    pure touch

touchStart :: Touch -> Ivory eff ()
touchStart Touch{..} = store start true

modePort :: GPIO_PERIPH -> GPIO_PIN -> GPIO_MODE -> Ivory eff ()
modePort gpio pin mode = do
    setOutputOptions gpio gpio_otype_pp gpio_ospeed_50mhz pin
    setMode gpio mode gpio_pupd_none pin

instance I.Touch Touch where
    getDebug Touch{..} = deref debugVal
    getState Touch{..} = deref stateTouch

resetAvg :: Touch -> Ivory eff ()
resetAvg Touch{..} = do
    store avg =<< deref avg_

runMeasurement :: Touch -> Ivory (ProcEffects s ()) ()
runMeasurement Touch{..} = do
    disableIRQ
    -- resetBit port pin
    modePort port pin gpio_mode_input
    I.resetCounter timer
    forever do
        isMeasured <- getInputBit port pin
        when isMeasured breakOut
    moment <- safeCast <$> I.getCounter timer
    enableIRQ
    modePort port pin gpio_mode_output
    resetBit port pin

    avg' <- deref avg
    store avg $ average 0.001 avg' moment

    start' <- deref start
    when
        start'
        do
            sum' <- deref sum
            store sum $ sum' + moment

            ix' <- deref ix
            store ix $ ix' + 1

            when (ix' ==? 300) do
                store ix 0

                value <- (/ 300) <$> deref sum
                store sum 0

                stateTouch' <- deref stateTouch

                let diff' = abs $ value - avg'
                when (diff' >? top .&& iNot stateTouch') do
                    store stateTouch true
                    store avg_ avg'
                    store diff diff'
                    store debugVal diff'

                avg_' <- deref avg_
                let diff_' = abs $ value - avg_'
                bottom' <- (* 0.75) <$> deref diff
                when (diff_' <? bottom' .&& stateTouch') do
                    store stateTouch false
                    store avg avg_'
                    store debugVal (-diff_')

                store debugVal bottom'

average :: IFloat -> IFloat -> IFloat -> IFloat
average alpha a b =
    a * (1 - alpha) + b * alpha
