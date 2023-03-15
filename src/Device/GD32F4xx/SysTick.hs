{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Device.GD32F4xx.SysTick where

import           Core.Include
import           Core.Initialize
import           Interface.Timer
import           Ivory.Language
import           Ivory.Language.Module
import           Support.CMSIS.CoreCM4


newtype SysTick = SysTick Uint32

sysTick = SysTick


instance Include (HandleTimer SysTick) where
    include (HandleTimer {..}) = incl (handleIRQ handle)


instance Initialize (HandleTimer SysTick) where
    initialize (HandleTimer {timer = (SysTick ticks)}) = [
            proc "systick_init" $ body $ sysTickConfig ticks
        ]

handleIRQ :: (forall eff. Ivory eff ()) -> Def('[] :-> ())
handleIRQ handle = proc "SysTick_Handler" $ body handle


instance Timer SysTick
