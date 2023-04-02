{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.EXTI
    ( EXTI_LINE
    , EXTI_MODE
    , EXTI_TRIG_TYPE

    , exti_1
    , exti_2
    , exti_3
    , exti_4
    , exti_5
    , exti_6
    , exti_7
    , exti_8
    , exti_9
    , exti_10
    , exti_11
    , exti_12
    , exti_13
    , exti_14
    , exti_15
    , exti_16
    , exti_17
    , exti_18
    , exti_19
    , exti_20
    , exti_21
    , exti_22
    , exti_23
    , exti_24
    , exti_25
    , exti_26
    , exti_27

    , exti_interrupt
    , exti_mode

    , exti_trig_rising
    , exti_trig_falling
    , exti_trig_both
    , exti_trig_none

    , initExti
    , getExtiInterruptFlag
    , clearExtiInterruptFlag

    , inclExti
    ) where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Support
import           Ivory.Support.Device.GD32F3x0

newtype EXTI_LINE = EXTI_LINE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryVar, IvoryType)

exti_1  = EXTI_LINE $ ext "EXTI_1"
exti_2  = EXTI_LINE $ ext "EXTI_2"
exti_3  = EXTI_LINE $ ext "EXTI_3"
exti_4  = EXTI_LINE $ ext "EXTI_4"
exti_5  = EXTI_LINE $ ext "EXTI_5"
exti_6  = EXTI_LINE $ ext "EXTI_6"
exti_7  = EXTI_LINE $ ext "EXTI_7"
exti_8  = EXTI_LINE $ ext "EXTI_8"
exti_9  = EXTI_LINE $ ext "EXTI_9"
exti_10 = EXTI_LINE $ ext "EXTI_10"
exti_11 = EXTI_LINE $ ext "EXTI_11"
exti_12 = EXTI_LINE $ ext "EXTI_12"
exti_13 = EXTI_LINE $ ext "EXTI_13"
exti_14 = EXTI_LINE $ ext "EXTI_14"
exti_15 = EXTI_LINE $ ext "EXTI_15"
exti_16 = EXTI_LINE $ ext "EXTI_16"
exti_17 = EXTI_LINE $ ext "EXTI_17"
exti_18 = EXTI_LINE $ ext "EXTI_18"
exti_19 = EXTI_LINE $ ext "EXTI_19"
exti_20 = EXTI_LINE $ ext "EXTI_20"
exti_21 = EXTI_LINE $ ext "EXTI_21"
exti_22 = EXTI_LINE $ ext "EXTI_22"
exti_23 = EXTI_LINE $ ext "EXTI_23"
exti_24 = EXTI_LINE $ ext "EXTI_24"
exti_25 = EXTI_LINE $ ext "EXTI_25"
exti_26 = EXTI_LINE $ ext "EXTI_26"
exti_27 = EXTI_LINE $ ext "EXTI_27"



newtype EXTI_MODE = EXTI_MODE Uint8
    deriving (IvoryExpr, IvoryInit, IvoryVar, IvoryType)

exti_interrupt = EXTI_MODE $ ext "EXTI_INTERRUPT"
exti_mode      = EXTI_MODE $ ext "EXTI_EVENT"



newtype EXTI_TRIG_TYPE = EXTI_TRIG_TYPE Uint8
    deriving (IvoryExpr, IvoryInit, IvoryVar, IvoryType)

exti_trig_rising  = EXTI_TRIG_TYPE $ ext "EXTI_TRIG_RISING"
exti_trig_falling = EXTI_TRIG_TYPE $ ext "EXTI_TRIG_FALLING"
exti_trig_both    = EXTI_TRIG_TYPE $ ext "EXTI_TRIG_BOTH"
exti_trig_none    = EXTI_TRIG_TYPE $ ext "EXTI_TRIG_NONE"




initExti :: EXTI_LINE -> EXTI_MODE -> EXTI_TRIG_TYPE -> Ivory eff ()
initExti = call_ exti_init

exti_init :: Def ('[EXTI_LINE, EXTI_MODE, EXTI_TRIG_TYPE] :-> ())
exti_init = fun "exti_init"


getExtiInterruptFlag :: EXTI_LINE -> Ivory eff IBool
getExtiInterruptFlag = call exti_interrupt_flag_get

exti_interrupt_flag_get :: Def ('[EXTI_LINE] :-> IBool)
exti_interrupt_flag_get = fun "exti_interrupt_flag_get"


clearExtiInterruptFlag :: EXTI_LINE -> Ivory eff ()
clearExtiInterruptFlag = call_ exti_interrupt_flag_clear

exti_interrupt_flag_clear :: Def ('[EXTI_LINE] :-> ())
exti_interrupt_flag_clear = fun "exti_interrupt_flag_clear"



inclExti :: ModuleDef
inclExti = do
    inclSym exti_1
    inclSym exti_2
    inclSym exti_3
    inclSym exti_4
    inclSym exti_5
    inclSym exti_6
    inclSym exti_7
    inclSym exti_8
    inclSym exti_9
    inclSym exti_10
    inclSym exti_11
    inclSym exti_12
    inclSym exti_13
    inclSym exti_14
    inclSym exti_15
    inclSym exti_16
    inclSym exti_17
    inclSym exti_18
    inclSym exti_19
    inclSym exti_20
    inclSym exti_21
    inclSym exti_22
    inclSym exti_23
    inclSym exti_24
    inclSym exti_25
    inclSym exti_26
    inclSym exti_27

    inclSym exti_interrupt
    inclSym exti_mode

    inclSym exti_trig_rising
    inclSym exti_trig_falling
    inclSym exti_trig_both
    inclSym exti_trig_none

    incl exti_init
    incl exti_interrupt_flag_get
    incl exti_interrupt_flag_clear
