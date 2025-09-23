module Support.CMSIS.CoreCM4 (
    sysTickConfig,
    nop,
    isb,
    dsb,
    dmb,
    inclCoreCM4,
) where

import Control.Monad
import Ivory.Language
import Ivory.Language.Proc
import Ivory.Language.Syntax

fun :: (ProcType f) => Sym -> Def f
fun = (`importProc` "core_cm4.h")

sysTickConfig :: Uint32 -> Ivory eff ()
sysTickConfig = call_ sysTick_Config

sysTick_Config :: Def ('[Uint32] :-> ())
sysTick_Config = fun "SysTick_Config"

nop :: Int -> Ivory eff ()
nop n = replicateM_ n (call_ __NOP)

__NOP :: Def ('[] :-> ())
__NOP = fun "__NOP"

isb :: Ivory eff ()
isb = call_ __ISB

__ISB :: Def ('[] :-> ())
__ISB = fun "__ISB"

dsb :: Ivory eff ()
dsb = call_ __DSB

__DSB :: Def ('[] :-> ())
__DSB = fun "__DSB"

dmb :: Ivory eff ()
dmb = call_ __DMB

__DMB :: Def ('[] :-> ())
__DMB = fun "__DMB"

inclCoreCM4 :: ModuleDef
inclCoreCM4 = do
    incl sysTick_Config
    incl __NOP
    incl __ISB
    incl __DSB
    incl __DMB
