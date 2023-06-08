{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F4xx.PWM where

import           Control.Monad.Writer
import           Core.Context
import           Core.Handler
import           Device.GD32F4xx.GPIO
import           Device.GD32F4xx.Timer
import qualified Interface.PWM                  as I
import qualified Interface.Timer                as I
import           Ivory.Language
import           Support.Device.GD32F4xx.System
import           Support.Device.GD32F4xx.Timer



pwmTimerParam :: Uint32 -> Uint32 -> Init (Struct TIMER_PARAM_STRUCT)
pwmTimerParam frequency' period' =
    timerParam [ prescaler .= ival (castDefault $ system_core_clock `iDiv` frequency' - 1)
               , period    .= ival (period' - 1)
               ]


pwm_timer_1 :: MonadWriter Context m => Uint32 -> Uint32 -> m Timer
pwm_timer_1 frequency' period' = timer_1 $ pwmTimerParam frequency' period'

pwm_timer_2 :: MonadWriter Context m => Uint32 -> Uint32 -> m Timer
pwm_timer_2 frequency' period' = timer_2 $ pwmTimerParam frequency' period'



data PWM = PWM
    { pwmTimer   :: Timer
    , pwmChannel :: TIMER_CHANNEL
    , port       :: Port
    }

mkPWM :: MonadWriter Context m
      => (Uint32 -> Uint32 -> m Timer)
      -> TIMER_CHANNEL
      -> Port
      -> Uint32
      -> Uint32
      -> m PWM
mkPWM timer' pwmChannel port frequency period = do
    pwmTimer <- timer' frequency period

    let initPWM' :: Def ('[] :-> ())
        initPWM' = proc (show port <> "_pwm_init") $ body $ do
            let t = timer pwmTimer
            initChannelOcTimer            t pwmChannel =<< local (istruct timerOcDefaultParam)
            configChannelOutputPulseValue t pwmChannel 0
            configTimerOutputMode         t pwmChannel timer_oc_mode_low
            configChannelOutputShadow     t pwmChannel timer_oc_shadow_disable
            configPrimaryOutput           t true
            enableTimer                   t

    addInit $ initPort port
    addInit initPWM'

    pure PWM { pwmTimer, pwmChannel, port }



instance I.PWM PWM where
    setDuty PWM{..} duty = do
        let t = timer pwmTimer
        configChannelOutputPulseValue t pwmChannel duty

    setMode PWM{..} = do
        let t = timer pwmTimer
        configTimerOutputMode t pwmChannel . coerceModePWM



coerceModePWM I.HIGH       = timer_oc_mode_pwm0
coerceModePWM I.LOW        = timer_oc_mode_pwm1
coerceModePWM I.FORCE_HIGH = timer_oc_mode_high
coerceModePWM I.FORCE_LOW  = timer_oc_mode_low



instance I.Timer PWM where
    setCounter PWM{..} = I.setCounter pwmTimer
    getCounter PWM{..} = I.getCounter pwmTimer



instance Handler I.HandleTimer PWM where
    addHandler I.HandleTimer {timer = PWM{..} , handle} =
        addHandler $ I.HandleTimer pwmTimer handle