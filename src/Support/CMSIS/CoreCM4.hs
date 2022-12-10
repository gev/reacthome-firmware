{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.CMSIS.CoreCM4
  ( nop
  , sysTickConfig
  , inclCoreCM4
  ) where

import           Control.Monad
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Language.Proc
import           Ivory.Language.Syntax
import           Ivory.Support
import           Ivory.Support.Device.GD32F3x0


inclCoreCM4 :: ModuleM ()
inclCoreCM4 = do
  incl __NOP
  incl sysTick_Config


nop :: Int -> Ivory eff ()
nop n = replicateM_ n (call_ __NOP)

__NOP :: Def ('[] :-> ())
__NOP = fun "__NOP"


sysTickConfig :: Uint32 -> Ivory eff ()
sysTickConfig = call_ sysTick_Config

sysTick_Config :: Def ('[Uint32] :-> ())
sysTick_Config = fun "SysTick_Config"
