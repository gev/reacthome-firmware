{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F4xx.Timer where

import           Control.Monad.State            (MonadState)
import           Core.Context
import           Core.Handler
import qualified Interface.Counter              as I
import qualified Interface.Timer                as I
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Stdlib
import           Ivory.Support
import           Support.Device.GD32F3x0.System (system_core_clock)
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.Misc
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.Timer



data Timer = Timer
    { timer :: TIMER_PERIPH
    , rcu   :: RCU_PERIPH
    , irq   :: IRQn
    }


timer_1 :: MonadState Context m => Init (Struct TIMER_PARAM_STRUCT) -> m Timer
timer_1 = mkTimer timer1 rcu_timer1 timer1_irqn

timer_2 :: MonadState Context m => Init (Struct TIMER_PARAM_STRUCT) -> m Timer
timer_2 = mkTimer timer2 rcu_timer2 timer2_irqn

timer_3 :: MonadState Context m => Init (Struct TIMER_PARAM_STRUCT) -> m Timer
timer_3 = mkTimer timer3 rcu_timer3 timer3_irqn

timer_6 :: MonadState Context m => Init (Struct TIMER_PARAM_STRUCT) -> m Timer
timer_6 = mkTimer timer6 rcu_timer6 timer6_irqn


timerConfig :: Uint32 -> Uint32 -> Init (Struct TIMER_PARAM_STRUCT)
timerConfig frequency' period' =
    timerParam [ prescaler .= ival (castDefault $ system_core_clock `iDiv` frequency' - 1)
               , period    .= ival (period' - 1)
               ]


cfg_timer_1 :: MonadState Context m => Uint32 -> Uint32 -> m Timer
cfg_timer_1 frequency' period' = timer_1 $ timerConfig frequency' period'

cfg_timer_2 :: MonadState Context m => Uint32 -> Uint32 -> m Timer
cfg_timer_2 frequency' period' = timer_2 $ timerConfig frequency' period'

cfg_timer_3 :: MonadState Context m => Uint32 -> Uint32 -> m Timer
cfg_timer_3 frequency' period' = timer_3 $ timerConfig frequency' period'

cfg_timer_6 :: MonadState Context m => Uint32 -> Uint32 -> m Timer
cfg_timer_6 frequency' period' = timer_6 $ timerConfig frequency' period'


mkTimer :: MonadState Context m
        => TIMER_PERIPH
        -> RCU_PERIPH
        -> IRQn
        -> Init (Struct TIMER_PARAM_STRUCT)
        -> m Timer
mkTimer timer rcu irq param = do
    addInit (symbol timer) $ do
            enablePeriphClock rcu
            deinitTimer       timer
            initTimer         timer =<< local param
            enableTimer       timer
    pure Timer { timer, rcu, irq }



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

instance I.Timer Timer where
    setCounter t = writeCounter (timer t)
    getCounter t = readCounter  (timer t)
