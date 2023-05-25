{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Device.GD32F3x0.PWM where

import           Control.Monad.Writer
import           Core.Context
import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.Timer
import qualified Interface.PWM                  as I
import qualified Interface.Timer                as T
import           Ivory.Language
import           Support.Device.GD32F3x0.System
import           Support.Device.GD32F3x0.Timer
import Core.Handler



pwmTimerParam :: Uint32 -> Uint32 -> Init (Struct TIMER_PARAM_STRUCT)
pwmTimerParam frequency' period' =
    timerParam [ prescaler .= ival (castDefault $ system_core_clock `iDiv` frequency' - 1)
               , period    .= ival (period' - 1)
               ]

pwm_timer_0 :: MonadWriter Context m => Uint32 -> Uint32 -> m Timer
pwm_timer_0 frequency' period' = timer_0 $ pwmTimerParam frequency' period'

pwm_timer_1 :: MonadWriter Context m => Uint32 -> Uint32 -> m Timer
pwm_timer_1 frequency' period' = timer_1 $ pwmTimerParam frequency' period'

pwm_timer_2 :: MonadWriter Context m => Uint32 -> Uint32 -> m Timer
pwm_timer_2 frequency' period' = timer_2 $ pwmTimerParam frequency' period'



data PWM = PWM
    { timer_pwm   :: Timer
    , channel_pwm :: TIMER_CHANNEL
    , port        :: Port
    }

mkPWM :: MonadWriter Context m => (Uint32 -> Uint32 -> m Timer) -> TIMER_CHANNEL -> Port -> Uint32 -> Uint32 -> m PWM
mkPWM timer' channel_pwm port frequency period = do
    timer_pwm <- timer' frequency period

    let initPWM' :: Def ('[] :-> ())
        initPWM' = proc (show port <> "_pwm_init") $ body $ do
            let t = timer timer_pwm
            initChannelOcTimer            t channel_pwm =<< local (istruct timerOcDefaultParam)
            configChannelOutputPulseValue t channel_pwm 0
            configTimerOutputMode         t channel_pwm timer_oc_mode_low
            configChannelOutputShadow     t channel_pwm timer_oc_shadow_disable
            configPrimaryOutput           t true
            enableTimer                   t

    addInit $ initPort port
    addInit initPWM'

    pure PWM { timer_pwm, channel_pwm, port }



instance I.PWM PWM where
    setDuty PWM{..} duty = do
        let t = timer timer_pwm
        configChannelOutputPulseValue t channel_pwm duty

    resetCounter PWM{..} = do
        T.setCounter timer_pwm 0

    setMode PWM{..} = do
        let t = timer timer_pwm
        configTimerOutputMode t channel_pwm . coerceModePWM


coerceModePWM I.HIGH       = timer_oc_mode_pwm0
coerceModePWM I.LOW        = timer_oc_mode_pwm1
coerceModePWM I.FORCE_HIGH = timer_oc_mode_high
coerceModePWM I.FORCE_LOW  = timer_oc_mode_low



instance T.Timer PWM where
    setCounter PWM{..} = T.setCounter timer_pwm
    getCounter PWM{..} = T.getCounter timer_pwm
    


instance Handler T.HandleTimer PWM where
    addHandler T.HandleTimer {timer = PWM{..} , handle} = 
        addHandler $ T.HandleTimer timer_pwm handle