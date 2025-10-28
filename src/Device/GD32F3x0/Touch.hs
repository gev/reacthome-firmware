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

type Samples = 10

data Touch = Touch
    { port :: GPIO_PERIPH
    , pin :: GPIO_PIN
    , timer :: Timer
    , threshold :: IFloat
    , sum :: Value IFloat
    , min :: Value IFloat
    , max :: Value IFloat
    , avg :: Value IFloat
    , ix :: Value (Ix Samples)
    , moments :: Values Samples Uint16
    , variance :: Value IFloat
    , variance_ :: Value IFloat
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
    m Touch
mkTouch port pin rcuPin threshold = do
    timer <- cfg_timer_14 84_000_000 0xffff_ffff

    let name = symbol port <> "_" <> symbol pin

    sum <- value ("touch_sum" <> name) 0
    min <- value ("touch_min" <> name) 0xffff_ffff
    max <- value ("touch_max" <> name) 0
    avg <- value ("touch_avg" <> name) 0
    variance <- value ("touch_variance" <> name) 0
    variance_ <- value ("touch_variance_" <> name) 0
    ix <- value ("touch_ix" <> name) 0
    moments <- values_ ("touch_moments" <> name)
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
                , threshold
                , sum
                , min
                , max
                , avg
                , ix
                , variance
                , variance_
                , moments
                , stateTouch
                , start
                , debugVal
                }

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

runMeasurement :: Touch -> Ivory (ProcEffects s ()) ()
runMeasurement Touch{..} = do
    disableIRQ
    modePort port pin gpio_mode_input
    I.resetCounter timer
    forever do
        isMeasured <- getInputBit port pin
        when isMeasured breakOut
    moment <- castDefault <$> I.getCounter timer
    enableIRQ
    modePort port pin gpio_mode_output
    resetBit port pin

    start' <- deref start
    ifte_
        start'
        do
            ix' <- deref ix
            store (moments ! ix') moment

            let moment' = safeCast moment

            sum' <- deref sum
            store sum $ sum' + moment'

            store ix $ ix' + 1
            ix'' <- deref ix

            when (ix'' ==? 0) do
                let n = arrayLen moments
                
                avg' <- deref avg
                avg_ <- (/ n) <$> deref sum
                store sum 0

                var <- local izero
                var_ <- local izero


                arrayMap \kx -> do
                    val <- safeCast <$> deref (moments ! kx)

                    var' <- deref var
                    let d = val - avg'
                    store var $ var' + (d * d)

                    var_' <- deref var_
                    let d_ = val - avg_
                    store var_ $ var_' + abs d_
                
                var' <- (/ avg') . (/ n) <$> deref var
                var_'' <- (/ n) <$> deref var_


                -- store debugVal $ var_' * 10000

                variance' <- deref variance
                store variance $ average 0.1 variance' var'
                variance'' <- deref variance

                variance_' <- deref variance_
                store variance_ $ average 0.01 variance_' var_''
                variance_'' <- deref variance_

                min' <- deref min
                when (min' >? variance_'') do
                    store min variance_''

                max' <- deref max
                when (max' <? variance_'') do
                    store max variance_''

                min'' <- deref min
                max'' <- deref max

                store debugVal $ (variance_'' - min'') / (max'' - min'') * 1000

                -- when (variance'' >? 0.6) do
                --     store stateTouch true
                --     store debugVal $ variance'' * 100
                -- when (variance'' <? 0.25) do
                --     store stateTouch false
                --     store debugVal $ (-variance'') * 100
                -- when (variance_'' / max'' <? 0.5) do
                --     avg' <- deref avg
                --     store avg $ average 0.001 avg' avg_

        do
            avg' <- deref avg
            store avg $ average 0.01 avg' $ safeCast moment


average :: IFloat -> IFloat -> IFloat -> IFloat
average alpha a b =
    a * (1 - alpha) + b * alpha
