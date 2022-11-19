{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.Timer
  ( TIMER_PERIPH            (..)
  , TIMER_ALIGNE_MODE       (..)
  , TIMER_COUNTER_DIRECTION (..)
  , TIMER_INT               (..)
  , TIMER_INT_FLAG          (..)
  , deinitTimer
  , enableTimer
  , enableTimerInterrupt
  , clearTimerInterruptFlag
  , inclTimer
  ) where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Language.Proxy
import           Ivory.Language.Syntax
import           Support.Ivory

(def, fun) = include "gd32f3x0_timer.h"


data TIMER_PERIPH
  = TIMER_0
  | TIMER_1
  | TIMER_2
  deriving (Show, Enum, Bounded)
instance ExtDef TIMER_PERIPH Uint32

data TIMER_ALIGNE_MODE
  = TIMER_COUNTER_EDGE
  deriving (Show, Enum, Bounded)
instance ExtDef TIMER_ALIGNE_MODE Uint16

data TIMER_COUNTER_DIRECTION
  = TIMER_COUNTER_UP
  | TIMER_COUNTER_DOWN
  deriving (Show, Enum, Bounded)
instance ExtDef TIMER_COUNTER_DIRECTION Uint16

data TIMER_INT
  = TIMER_UP
  deriving (Show, Enum, Bounded)
instance ExtDef TIMER_INT Uint32

data TIMER_INT_FLAG
  = TIMER_FLAG_UP
  deriving (Show, Enum, Bounded)
instance ExtDef TIMER_INT_FLAG Uint32


inclTimer :: ModuleM ()
inclTimer = do
  inclDef (def :: Cast TIMER_PERIPH Uint32)
  inclDef (def :: Cast TIMER_ALIGNE_MODE Uint16)
  inclDef (def :: Cast TIMER_COUNTER_DIRECTION Uint16)
  incl timer_struct_para_init
  incl timer_deinit
  incl timer_enable
  defStruct timer_parameter_struct


deinitTimer :: TIMER_PERIPH -> Ivory eff ()
deinitTimer = call_ timer_deinit . def

timer_deinit :: Def ('[Uint32] :-> ())
timer_deinit = fun "timer_deinit"


enableTimer :: TIMER_PERIPH -> Ivory eff ()
enableTimer = call_ timer_enable . def

timer_enable :: Def ('[Uint32] :-> ())
timer_enable = fun "timer_enable"


enableTimerInterrupt :: TIMER_PERIPH -> TIMER_INT -> Ivory eff ()
enableTimerInterrupt t i = call_ timer_interrupt_enable (def t) (def i)

timer_interrupt_enable :: Def ('[Uint32, Uint32] :-> ())
timer_interrupt_enable = fun "timer_interrupt_enable"


clearTimerInterruptFlag :: TIMER_PERIPH -> TIMER_INT_FLAG -> Ivory eff ()
clearTimerInterruptFlag t i = call_ timer_interrupt_flag_clear (def t) (def i)

timer_interrupt_flag_clear :: Def ('[Uint32, Uint32] :-> ())
timer_interrupt_flag_clear = fun "timer_interrupt_flag_clear"


timer_struct_para_init :: Def ('[Ref s (Stored ())] :-> ())
timer_struct_para_init = fun "timer_struct_para_init"

timer_parameter_struct = Proxy :: Proxy "timer_parameter_struct"
[ivory|
  struct timer_parameter_struct
    { prescaler         :: Stored Uint16
    ; alignedmode       :: Stored Uint16
    ; counterdirection  :: Stored Uint16
    ; clockdivision     :: Stored Uint16
    ; period            :: Stored Uint32
    ; repetitioncounter :: Stored Uint8
    }
|]
