{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Device.GD32F4xx.SysTick where

import Control.Monad.State
import Core.Context
import Core.Handler
import Interface.Timer
import Ivory.Language
import Ivory.Language.Module
import Support.CMSIS.CoreCM4

newtype SysTick = SysTick Uint32

sysTick :: (MonadState Context m) => Uint32 -> m SysTick
sysTick freq = do
    addInit "systick_init" $ sysTickConfig freq
    pure $ SysTick freq

instance Handler HandleTimer SysTick where
    addHandler (HandleTimer{timer = (SysTick ticks), handle}) = do
        addProc handleIRQ'
      where
        handleIRQ' :: Def ('[] :-> ())
        handleIRQ' = proc "SysTick_Handler" $ body handle

instance Timer SysTick where
    setCounter _ _ = pure ()
    getCounter _ = pure 0
    enableInterrupt _ = pure ()
    disableInterrupt _ = pure ()
