{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F3x0.NeoPixel where

import           Control.Monad.Writer
import           Core.Context
import           Core.Handler
import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.Timer
import qualified Interface.PWM                  as I
import qualified Interface.Timer                as T
import           Ivory.Language
import           Support.Device.GD32F3x0.System
import           Support.Device.GD32F3x0.Timer



data NeoPixel = NeoPixel
    { timer_pwm   :: Timer
    , channel_pwm :: TIMER_CHANNEL
    , port        :: Port
    }

mkNeoPixel :: MonadWriter Context m
           => (Uint32 -> Uint32 -> m Timer)
           -> TIMER_CHANNEL
           -> Port
           -> m NeoPixel
mkNeoPixel timer' channel_pwm port frequency period = do
    timer_pwm <- timer' frequency period

    let initNeoPixel' :: Def ('[] :-> ())
        initNeoPixel' = proc (show port <> "_pwm_init") $ body $ do
            let t = timer timer_pwm
            initChannelOcTimer            t channel_pwm =<< local (istruct timerOcDefaultParam)
            configChannelOutputPulseValue t channel_pwm 0
            configTimerOutputMode         t channel_pwm timer_oc_mode_low
            configChannelOutputShadow     t channel_pwm timer_oc_shadow_disable
            configPrimaryOutput           t true
            enableTimer                   t

    addInit $ initPort port
    addInit initNeoPixel'

    pure NeoPixel { timer_pwm, channel_pwm, port }
