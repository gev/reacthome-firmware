{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE RankNTypes            #-}

module Support.Device.GD32F3x0.Timer
  ( TIMER_PERIPH            (..)
  , TIMER_ALIGNE_MODE       (..)
  , TIMER_COUNTER_DIRECTION (..)
  , TIMER_CLOCK_DIVISION    (..)
  , TIMER_INT               (..)
  , TIMER_INT_FLAG          (..)
  , TIMER_PARAM             (..)
  , deinitTimer
  , enableTimer
  , enableTimerInterrupt
  , getTimerInterruptFlag
  , clearTimerInterruptFlag
  , timerParam
  , initTimer
  , inclTimer
  ) where

import           Data.Function                 ((&))
import           Data.Maybe
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Language.Uint
import           Ivory.Support
import           Ivory.Support.Device.GD32F3x0


data TIMER_PERIPH
  = TIMER0
  | TIMER1
  | TIMER2
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
  = TIMER_INT_UP
  deriving (Show, Enum, Bounded)
instance ExtDef TIMER_INT Uint32

data TIMER_INT_FLAG
  = TIMER_INT_FLAG_UP
  deriving (Show, Enum, Bounded)
instance ExtDef TIMER_INT_FLAG Uint32

data TIMER_CLOCK_DIVISION
  = TIMER_CKDIV_DIV1
  | TIMER_CKDIV_DIV2
  | TIMER_CKDIV_DIV4
  deriving (Show, Enum, Bounded)
instance ExtDef TIMER_CLOCK_DIVISION Uint16

data TIMER_PARAM = TIMER_PARAM
    { timerPrescaler         :: Uint16
    , timerAlignedMode       :: TIMER_ALIGNE_MODE
    , timerCounterDirection  :: TIMER_COUNTER_DIRECTION
    , timerClockDivision     :: TIMER_CLOCK_DIVISION
    , timerPeriod            :: Uint32
    , timerRepetitionCounter :: Uint8
    }

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


inclTimer :: ModuleM ()
inclTimer = do
  inclDef (def :: Cast TIMER_PERIPH Uint32)
  inclDef (def :: Cast TIMER_ALIGNE_MODE Uint16)
  inclDef (def :: Cast TIMER_COUNTER_DIRECTION Uint16)
  inclDef (def :: Cast TIMER_CLOCK_DIVISION Uint16)
  inclDef (def :: Cast TIMER_INT Uint32)
  inclDef (def :: Cast TIMER_INT_FLAG Uint32)
  incl timer_interrupt_flag_get
  incl timer_interrupt_flag_clear
  incl timer_interrupt_enable
  incl timer_deinit
  incl timer_enable
  incl timer_init


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


getTimerInterruptFlag :: TIMER_PERIPH -> TIMER_INT_FLAG -> Ivory eff IBool
getTimerInterruptFlag t i =
  call timer_interrupt_flag_get (def t) (def i)

timer_interrupt_flag_get :: Def ('[Uint32, Uint32] :-> IBool)
timer_interrupt_flag_get = fun "timer_interrupt_flag_get"


clearTimerInterruptFlag :: TIMER_PERIPH -> TIMER_INT_FLAG -> Ivory eff ()
clearTimerInterruptFlag t i = call_ timer_interrupt_flag_clear (def t) (def i)

timer_interrupt_flag_clear :: Def ('[Uint32, Uint32] :-> ())
timer_interrupt_flag_clear = fun "timer_interrupt_flag_clear"


initTimer :: TIMER_PERIPH -> TIMER_PARAM -> (forall s. Ivory (ProcEffects s ()) ())
initTimer t p = do
  r <- local $ istruct
    [ prescaler         .= ival (p & timerPrescaler)
    , alignedmode       .= ival (p & timerAlignedMode & def)
    , counterdirection  .= ival (p & timerCounterDirection & def)
    , clockdivision     .= ival (p & timerClockDivision & def)
    , period            .= ival (p & timerPeriod)
    , repetitioncounter .= ival (p & timerRepetitionCounter)
    ]
  call_ timer_init (def t) r

timer_init :: Def ('[Uint32, Ref s ('Struct "timer_parameter_struct")] :-> ())
timer_init = fun "timer_init"


timerParam :: TIMER_PARAM
timerParam = TIMER_PARAM  0
                          TIMER_COUNTER_EDGE
                          TIMER_COUNTER_UP
                          TIMER_CKDIV_DIV1
                          65535
                          0
