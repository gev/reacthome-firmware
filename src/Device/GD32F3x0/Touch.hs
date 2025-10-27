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

type Samples = 300

data Touch = Touch
    { port :: GPIO_PERIPH
    , pin :: GPIO_PIN
    , timer :: Timer
    , threshold :: IFloat
    , sum :: Value IFloat
    , max :: Value IFloat
    , min :: Value IFloat
    , ix :: Value (Ix Samples)
    , buffMoments :: Values Samples Uint16
    , variance :: Value IFloat
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
    max <- value ("touch_max" <> name) 0
    min <- value ("touch_min" <> name) 0xffff_ffff
    variance <- value ("touch_variance" <> name) 0
    ix <- value ("touch_ix" <> name) 0
    buffMoments <- values_ ("touch_moments" <> name)
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
                , max
                , min
                , ix
                , variance
                , buffMoments
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
    when
        start'
        do
            ix' <- deref ix
            store (buffMoments ! ix') moment

            let moment' = safeCast moment

            sum' <- deref sum
            store sum $ sum' + moment'


            store ix $ ix' + 1
            ix'' <- deref ix

            when (ix'' ==? 0) do
                value <- (/ arrayLen buffMoments) <$> deref sum
                store sum 0

                max' <- deref max

                store variance 0
                arrayMap \i -> do
                    v <- safeCast <$> deref (buffMoments ! i)
                    variance' <- deref variance
                    let dv = v - value 
                    store variance $ abs dv + variance'


                -- stateTouch' <- deref stateTouch

                variance' <- (/ arrayLen buffMoments) <$> deref variance
                max' <- deref max
                when (variance' >? max') do
                    store max variance'

                max'' <- deref max

                let variance'' = variance' / max''

                when (variance'' >? 0.6) do
                    store stateTouch true
                when (variance'' <? 0.4) do
                    store stateTouch false
                store debugVal (variance'' * 1000)

average :: IFloat -> IFloat -> IFloat -> IFloat
average alpha a b =
    a * (1 - alpha) + b * alpha
