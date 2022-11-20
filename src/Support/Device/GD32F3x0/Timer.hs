{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

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
  , defaultTimerParam
  , initTimer
  , inclTimer
  ) where

import           Data.Maybe
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Language.Uint   (Uint16 (Uint16))
import           Support.Ivory

(def, fun) = include "gd32f3x0_timer.h"


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
    { prescaler         :: Maybe Uint16
    , alignedMode       :: Maybe TIMER_ALIGNE_MODE
    , counterDirection  :: Maybe TIMER_COUNTER_DIRECTION
    , clockDivision     :: Maybe TIMER_CLOCK_DIVISION
    , period            :: Maybe Uint32
    , repetitionCounter :: Maybe Uint8
    }

[ivory|
  struct timer_param
    { prescaler_         :: Stored Uint16
    ; aligned_mode       :: Stored Uint16
    ; counter_direction  :: Stored Uint16
    ; clock_division     :: Stored Uint16
    ; period_            :: Stored Uint32
    ; repetition_counter :: Stored Uint8
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
  incl timer_struct_para_init
  defStruct (Proxy :: Proxy "timer_param")



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

initTimer :: TIMER_PERIPH -> TIMER_PARAM -> Def ('[] :-> ())
initTimer t p = proc "init_timer" $ body $ do
      r <- local (istruct [])
      call_ timer_struct_para_init r
      let go ref get cast | isJust v  = store (r ~> ref) . cast . fromJust $ v
                          | otherwise = pure  ()
                          where v = get p
      go prescaler_ prescaler id
      go aligned_mode alignedMode def
      go counter_direction counterDirection def
      go clock_division clockDivision def
      go period_  period id
      go repetition_counter repetitionCounter id
      call_ timer_init (def t) r


timer_init :: Def ('[Uint32, Ref s ('Struct "timer_param")] :-> ())
timer_init = fun "timer_init"

timer_struct_para_init :: Def ('[Ref s ('Struct "timer_param")] :-> ())
timer_struct_para_init = fun "timer_struct_para_init"

defaultTimerParam :: TIMER_PARAM
defaultTimerParam = TIMER_PARAM Nothing Nothing Nothing Nothing Nothing Nothing
