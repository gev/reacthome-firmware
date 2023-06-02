{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F4xx.Timer where

import           Control.Monad.Writer          (MonadWriter)
import           Core.Context
import           Core.Handler
import qualified Interface.Counter             as I
import qualified Interface.Timer               as I
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Stdlib
import           Ivory.Support
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.Misc
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.Timer



data Timer = forall s. Timer
    { timer :: TIMER_PERIPH
    , rcu   :: RCU_PERIPH
    , irq   :: IRQn
    }



timer_1 :: MonadWriter Context m => Init (Struct TIMER_PARAM_STRUCT) -> m Timer
timer_1 = mkTimer timer1 rcu_timer1 timer1_irqn

timer_2 :: MonadWriter Context m => Init (Struct TIMER_PARAM_STRUCT) -> m Timer
timer_2 = mkTimer timer2 rcu_timer2 timer2_irqn



mkTimer :: MonadWriter Context m
        => TIMER_PERIPH
        -> RCU_PERIPH
        -> IRQn
        -> Init (Struct TIMER_PARAM_STRUCT)
        -> m Timer
mkTimer timer rcu irq param = do
    addInit initTimer'
    pure Timer { timer, rcu, irq }
    where
        initTimer' :: Def ('[] ':-> ())
        initTimer' = proc (symbol timer <> "_init") $ body $ do
            enablePeriphClock rcu
            deinitTimer       timer
            initTimer         timer =<< local param
            enableTimer       timer



instance I.Counter Timer where
    readCounter t = castDefault <$> readCounter (timer t)



instance Handler I.HandleTimer Timer where
    addHandler (I.HandleTimer {I.timer = Timer{..}, handle}) = do
        addProc initTimerIRQ'
        addModule $ makeIRQHandler irq handleIRQ'
        where
            initTimerIRQ' :: Def ('[] ':-> ())
            initTimerIRQ' = proc (symbol timer <> "_irq_init") $ body $ do
                enableIrqNvic irq 0 0
                enableTimerInterrupt timer timer_int_up

            handleIRQ' :: Ivory eff ()
            handleIRQ' = do
                flag <- getTimerInterruptFlag timer timer_int_flag_up
                when flag $ clearTimerInterruptFlag timer timer_int_flag_up
                handle
