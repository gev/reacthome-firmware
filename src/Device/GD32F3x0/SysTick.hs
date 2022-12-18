{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Device.GD32F3x0.SysTick where

import           Interface
import           Interface.Timer
import           Ivory.Language
import           Ivory.Language.Module
import           Support.CMSIS.CoreCM4


newtype SysTick = SysTick Uint32

sysTick = SysTick


instance Interface (HandleTimer SysTick) where

  dependencies (HandleTimer {handle}) =
    incl (handleIRQ handle) : inclCoreCM4

  initialize (HandleTimer {timer = (SysTick ticks)}) = [
      proc "systick_init" $ body $ sysTickConfig ticks
    ]

handleIRQ :: (forall eff. Ivory eff ()) -> Def('[] :-> ())
handleIRQ handle = proc "SysTick_Handler" $ body handle


instance Timer SysTick
