{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.EXTI
    ( EXTI_LINE(..)
    , EXTI_MODE(..)
    , EXTI_TRIG_TYPE(..)
    , initExti
    , getExtiInterruptFlag
    , clearExtiInterruptFlag
    ) where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Support
import           Ivory.Support.Device.GD32F3x0

data EXTI_LINE  = EXTI_0 
                | EXTI_1 
                | EXTI_2 
                | EXTI_3 
                | EXTI_4 
                | EXTI_5 
                | EXTI_6 
                | EXTI_7 
                | EXTI_8 
                | EXTI_9 
                | EXTI_10
                | EXTI_11
                | EXTI_12
                | EXTI_13
                | EXTI_14
                | EXTI_15
                | EXTI_16
                | EXTI_17
                | EXTI_18
                | EXTI_19
                | EXTI_20
                | EXTI_21
                | EXTI_22
                | EXTI_23
                | EXTI_24
                | EXTI_25
                | EXTI_26
                | EXTI_27
        deriving (Show, Enum, Bounded)
instance ExtDef EXTI_LINE Uint32

data EXTI_MODE = EXTI_INTERRUPT
               | EXTI_EVENT
        deriving (Show, Enum, Bounded)
instance ExtDef EXTI_MODE Uint8

data EXTI_TRIG_TYPE = EXTI_TRIG_RISING
                    | EXTI_TRIG_FALLING
                    | EXTI_TRIG_BOTH
                    | EXTI_TRIG_NONE
        deriving (Show, Enum, Bounded)
instance ExtDef EXTI_TRIG_TYPE Uint8


inclExti :: ModuleM ()
inclExti = do
    inclDef (def :: Cast EXTI_LINE Uint32)
    inclDef (def :: Cast EXTI_MODE Uint8)
    inclDef (def :: Cast EXTI_TRIG_TYPE Uint8)
    incl exti_init
    incl exti_interrupt_flag_get
    incl exti_interrupt_flag_clear


initExti :: EXTI_LINE -> EXTI_MODE -> EXTI_TRIG_TYPE -> Ivory eff ()
initExti l m t = call_ exti_init (def l) (def m) (def t)

exti_init :: Def ('[Uint32, Uint8, Uint8] :-> ())
exti_init = fun "exti_init"


getExtiInterruptFlag :: EXTI_LINE -> Ivory eff IBool
getExtiInterruptFlag l = call exti_interrupt_flag_get (def l)

exti_interrupt_flag_get :: Def ('[Uint32] :-> IBool)
exti_interrupt_flag_get = fun "exti_interrupt_flag_get"


clearExtiInterruptFlag :: EXTI_LINE -> Ivory eff ()
clearExtiInterruptFlag l = call_ exti_interrupt_flag_clear (def l)

exti_interrupt_flag_clear :: Def ('[Uint32] :-> ())
exti_interrupt_flag_clear = fun "exti_interrupt_flag_clear"
