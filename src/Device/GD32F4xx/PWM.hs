module Device.GD32F4xx.PWM where

import Control.Monad.State
import Core.Context
import Core.Handler
import Device.GD32F4xx.GPIO.Port
import Device.GD32F4xx.Timer
import Interface.PWM qualified as I
import Interface.Timer qualified as I
import Ivory.Language
import Support.Device.GD32F4xx.GPIO (GPIO_PUPD, gpio_pupd_none)
import Support.Device.GD32F4xx.Timer

{--
    TODO: Timer might has a prescaler/multiplicator. So resulting frequency maybe wrong
--}

data PWM = PWM
    { pwmTimer :: Timer
    , pwmChannel :: TIMER_CHANNEL
    , port :: Port
    }

mkPWM ::
    (MonadState Context m) =>
    (Uint32 -> Uint32 -> m Timer) ->
    TIMER_CHANNEL ->
    (GPIO_PUPD -> Port) ->
    Uint32 ->
    Uint32 ->
    m PWM
mkPWM timer' pwmChannel port' frequency period = do
    pwmTimer <- timer' frequency period

    let port = port' gpio_pupd_none

    initPort port
    addInit (show port <> "_pwm") do
        let t = timer pwmTimer
        initChannelOcTimer t pwmChannel =<< local (istruct timerOcDefaultParam)
        configChannelOutputPulseValue t pwmChannel 0
        configTimerOutputMode t pwmChannel timer_oc_mode_low
        configChannelOutputShadow t pwmChannel timer_oc_shadow_disable
        configPrimaryOutput t true
        enableTimer t

    pure PWM{pwmTimer, pwmChannel, port}

instance I.PWM PWM where
    setDuty PWM{..} duty = do
        let t = timer pwmTimer
        configChannelOutputPulseValue t pwmChannel duty

    setMode PWM{..} = do
        let t = timer pwmTimer
        configTimerOutputMode t pwmChannel . coerceModePWM

coerceModePWM I.HIGH = timer_oc_mode_pwm0
coerceModePWM I.LOW = timer_oc_mode_pwm1
coerceModePWM I.FORCE_HIGH = timer_oc_mode_high
coerceModePWM I.FORCE_LOW = timer_oc_mode_low

instance I.Timer PWM where
    setCounter PWM{..} = I.setCounter pwmTimer
    getCounter PWM{..} = I.getCounter pwmTimer
    enableInterrupt PWM{..} = I.enableInterrupt pwmTimer
    disableInterrupt PWM{..} = I.disableInterrupt pwmTimer

instance Handler I.HandleTimer PWM where
    addHandler I.HandleTimer{timer = PWM{..}, handle} =
        addHandler $ I.HandleTimer pwmTimer handle
