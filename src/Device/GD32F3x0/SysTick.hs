{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F3x0.SysTick where

import           Control.Monad.Writer
import           Core.Context
import           Core.Handler
import           Interface.Timer
import           Ivory.Language
import           Ivory.Language.Module
import           Support.CMSIS.CoreCM4


newtype SysTick = SysTick Uint32

sysTick :: MonadWriter Context m => Uint32 -> m SysTick
sysTick freq = do
    addInit initSysTick'
    pure $ SysTick freq
    where
        initSysTick' :: Def ('[] :-> ())
        initSysTick' =  proc "systick_init" $ body $ sysTickConfig freq


instance Handler HandleTimer SysTick where
    addHandler (HandleTimer {timer = (SysTick ticks), handle}) = do
        addProc handleIRQ'
        where
            handleIRQ' :: Def('[] :-> ())
            handleIRQ' = proc "SysTick_Handler" $ body handle


instance Timer SysTick
