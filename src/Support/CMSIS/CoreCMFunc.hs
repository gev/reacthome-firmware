{-# HLINT ignore "Use camelCase" #-}

module Support.CMSIS.CoreCMFunc (
    enableIRQ,
    disableIRQ,
    setMSP,
    inclCoreCMFunc,
) where

import Ivory.Language
import Ivory.Language.Proc
import Ivory.Language.Syntax

fun :: (ProcType f) => Sym -> Def f
fun = (`importProc` "core_cmFunc.h")

enableIRQ :: Ivory eff ()
enableIRQ = call_ __enable_irq

__enable_irq :: Def ('[] :-> ())
__enable_irq = fun "__enable_irq"

disableIRQ :: Ivory eff ()
disableIRQ = call_ __disable_irq

__disable_irq :: Def ('[] :-> ())
__disable_irq = fun "__disable_irq"

setMSP :: Uint32 -> Ivory eff () 
setMSP = call_ __set_MSP

__set_MSP :: Def ('[Uint32] :-> ())
__set_MSP = fun "__set_MSP"

inclCoreCMFunc :: ModuleDef
inclCoreCMFunc = do
    incl __enable_irq
    incl __disable_irq
    incl __set_MSP
