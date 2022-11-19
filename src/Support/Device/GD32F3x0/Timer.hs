{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Ivory.Language.Syntax
import           Support.Ivory

(def, fun) = include "gd32f3x0_timer.h"

{--
typedef struct {
    uint16_t prescaler;                                                             /*!< prescaler value */
    uint16_t alignedmode;                                                           /*!< aligned mode */
    uint16_t counterdirection;                                                      /*!< counter direction */
    uint16_t clockdivision;                                                         /*!< clock division value */
    uint32_t period;                                                                /*!< period value */
    uint8_t  repetitioncounter;                                                     /*!< the counter repetition value */
} timer_parameter_struct;
--}

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
  -- incl timer_parameter_struct
  incl timer_deinit
  incl timer_enable


-- timer_parameter_struct :: IvoryExpr t => t
-- timer_parameter_struct =  def "timer_parameter_struct"


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
