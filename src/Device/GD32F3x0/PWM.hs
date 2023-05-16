{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeOperators    #-}

module Device.GD32F3x0.PWM where

import           Control.Monad.Writer
import           Core.Context
import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.Timer
import qualified Interface.PWM                 as I
import           Ivory.Language
import           Support.Device.GD32F3x0.Timer

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

    let pwm = PWM { timer_pwm, channel_pwm, port }

    let initPWM' :: Def ('[] :-> ())
        initPWM' = proc (show port <> "_pwm_init") $ body $ do
            let t = timer timer_pwm
            initChannelOcTimer            t channel_pwm =<< local (istruct timerOcDefaultParam)
            configChannelOutputPulseValue t channel_pwm 0
            configTimerOutputMode         t channel_pwm timer_oc_mode_pwm0
            configChannelOutputShadow     t channel_pwm timer_oc_shadow_disable
            configPrimaryOutput           t true
            enableTimer                   t
            I.setDuty pwm 127

    addInit $ initPort port
    addInit initPWM'
    pure pwm



instance I.PWM PWM where

    setDuty PWM{..} duty = do
        let t = timer timer_pwm
        configChannelOutputPulseValue t channel_pwm duty
