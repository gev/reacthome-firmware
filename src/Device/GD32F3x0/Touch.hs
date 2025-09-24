module Device.GD32F3x0.Touch where

import Control.Monad (replicateM_)
import Control.Monad.State
import Core.Context
import Core.Handler
import Core.Task
import Data.ByteString (index)
import Data.Value
import Device.GD32F3x0.EXTI
import Device.GD32F3x0.Timer
import Interface.Counter (Counter (readCounter))
import Interface.Timer qualified as I
import Interface.Touch qualified as I
import Ivory.Language hiding (setBit)
import Ivory.Language.Uint (Uint32 (Uint32))
import Ivory.Stdlib (ifte, when)
import Ivory.Stdlib.Control
import Ivory.Support (symbol)
import Support.CMSIS.CoreCMFunc (disableIRQ, enableIRQ)
import Support.Device.GD32F3x0.EXTI
import Support.Device.GD32F3x0.GPIO
import Support.Device.GD32F3x0.IRQ
import Support.Device.GD32F3x0.Misc
import Support.Device.GD32F3x0.RCU
import Support.Device.GD32F3x0.SYSCFG

data Touch = Touch
    { port :: GPIO_PERIPH
    , pin :: GPIO_PIN
    , timer :: Timer
    , threshold :: IFloat
    , avg :: Value IFloat
    , min :: Value IFloat
    , max :: Value IFloat
    , stateTouch :: Value IBool
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

    avg <- value ("touch_avg" <> name) 0
    min <- value ("touch_min" <> name) 0xffff_ffff
    max <- value ("touch_max" <> name) 0
    stateTouch <- value ("touch_state_touch" <> name) false
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
                , avg
                , min
                , max
                , stateTouch
                , debugVal
                }

    addTask $ delay 1_000 ("touch_reset_bounds" <> name) $ resetBounds touch
    addTask $ yeld ("touch_run" <> name) $ runMeasurement touch

    pure touch

modePort :: GPIO_PERIPH -> GPIO_PIN -> GPIO_MODE -> Ivory eff ()
modePort gpio pin mode = do
    setOutputOptions gpio gpio_otype_pp gpio_ospeed_50mhz pin
    setMode gpio mode gpio_pupd_none pin

instance I.Touch Touch where
    getDebug Touch{..} = deref debugVal
    getState Touch{..} = deref stateTouch

resetBounds :: Touch -> Ivory eff ()
resetBounds Touch{..} = do
    store min 0xffff_ffff
    store max 0

runMeasurement :: Touch -> Ivory (ProcEffects s ()) ()
runMeasurement Touch{..} = do
    disableIRQ
    resetBit port pin
    modePort port pin gpio_mode_input
    I.resetCounter timer
    forever do
        isMeasured <- getInputBit port pin
        when isMeasured breakOut
    moment <- safeCast <$> I.getCounter timer
    modePort port pin gpio_mode_output
    enableIRQ
    setBit port pin

    avg' <- deref avg
    store avg $ average 0.01 avg' moment
    avg'' <- deref avg

    min' <- deref min
    when (avg'' <? min') do
        store min avg''
    min'' <- deref min

    max' <- deref max
    when (avg'' >? max') do
        store max avg''
    max'' <- deref max

    let rising = avg'' - min''
    let falling = max'' - avg''

    cond_
        [ rising >? threshold ==> do
            store stateTouch true
            store debugVal rising
        , falling >? threshold
            * 0.6 ==> do
                store stateTouch false
                store debugVal (-falling)
        , true ==> store debugVal 0
        ]

average :: IFloat -> IFloat -> IFloat -> IFloat
average alpha a b =
    a * (1 - alpha) + b * alpha
