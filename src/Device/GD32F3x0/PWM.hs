{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Device.GD32F3x0.PWM where

import           Device.GD32F3x0.Timer     
import           Device.GD32F3x0.GPIO
import           Support.Device.GD32F3x0.Timer
import           Control.Monad.Writer
import           Core.Context
import           Ivory.Language

pwm_timer_0 :: MonadWriter Context m => m Timer
pwm_timer_0 = timer_0 $ timerParam [ prescaler .= ival 328
                                   , period    .= ival 255
                                   ]

pwm_timer_1 :: MonadWriter Context m => m Timer
pwm_timer_1 = timer_1 $ timerParam [ prescaler .= ival 328
                                   , period    .= ival 255
                                   ]

pwm_timer_2 :: MonadWriter Context m => m Timer
pwm_timer_2 = timer_2 $ timerParam [ prescaler .= ival 328
                                   , period    .= ival 255
                                   ]


data PWM = PWM
    { timer_pwm   :: Timer
    , channel_pwm :: TIMER_CHANNEL
    , port        :: Port
    }

mkPWM :: MonadWriter Context m => m Timer -> TIMER_CHANNEL -> Port -> m PWM
mkPWM timer' channel_pwm port = do
    timer_pwm <- timer'
    addInit $ initPort port
    let initPWM' :: Def ('[] :-> ()) 
        initPWM' = do
            let t = timer timer_pwm
            -- initChannelOcTimer t channel_pwm =<< local (istruct timerOcDefaultParam)
            -- configChannelOutputPulseValue t channel_pwm 0
            -- configTimerOutputMode t channel_pwm timer_oc_mode_pwm0
            -- configChannelOutputShadow t channel_pwm timer_oc_shadow_disable
            -- configPrimaryOutput t true
            enableTimer t


            
    addInit initPWM'
    pure $ PWM { timer_pwm, channel_pwm, port }

