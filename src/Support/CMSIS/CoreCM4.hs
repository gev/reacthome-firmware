{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.CMSIS.CoreCM4
    ( sysTickConfig
    , nop
    , isb
    , dsb
    , dmb

    , inclCoreCM4
    ) where

import           Control.Monad
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Language.Proc
import           Ivory.Language.Syntax
import           Ivory.Support



sysTickConfig :: Uint32 -> Ivory eff ()
sysTickConfig = call_ sysTick_Config

sysTick_Config :: Def ('[Uint32] :-> ())
sysTick_Config = importProc "SysTick_Config" "core_cm4.h"


nop :: Int -> Ivory eff ()
nop n = replicateM_ n (call_ __NOP)

__NOP :: Def ('[] :-> ())
__NOP = importProc "__NOP" "core_cm4.h"


isb :: Ivory eff ()
isb = call_ __ISB

__ISB :: Def ('[] :-> ())
__ISB = importProc "__ISB" "core_cm4.h"


dsb :: Ivory eff ()
dsb = call_ __DSB

__DSB :: Def ('[] :-> ())
__DSB = importProc "__DSB" "core_cm4.h"


dmb :: Ivory eff ()
dmb = call_ __DMB

__DMB :: Def ('[] :-> ())
__DMB = importProc "__DMB" "core_cm4.h"



inclCoreCM4 :: ModuleDef
inclCoreCM4 = do
    incl sysTick_Config
    incl __NOP
    incl __ISB
    incl __DSB
    incl __DMB
