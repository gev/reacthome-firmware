{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.Timer
    ( TIMER_PERIPH                      (..)
    , TIMER_ALIGNE_MODE                 (..)
    , TIMER_COUNTER_DIRECTION           (..)
    , TIMER_CLOCK_DIVISION              (..)
    , TIMER_INT                         (..)
    , TIMER_INT_FLAG                    (..)
    , TIMER_PARAM                       (..)
    , TIMER_CHANNEL                     (..)
    , TIMER_COMPARE_MODE                (..)
    , TIMER_COMPARE_SHADOW              (..)
    , TIMER_CHANNEL_STATE               (..)
    , TIMER_COMPL_CHANNEL_STATE         (..)
    , TIMER_CHANNEL_POLARITY            (..)
    , TIMER_COMPL_CHANNEL_POLARITY      (..)
    , TIMER_CHANNEL_IDLE_STATE          (..)
    , TIMER_COMPL_CHANNEL_IDLE_STATE    (..)
    , TIMER_OC_PARAM                    (..)
    , deinitTimer
    , enableTimer
    , enableTimerInterrupt
    , getTimerInterruptFlag
    , clearTimerInterruptFlag
    , timerParam
    , initTimer
    , readCounter
    , configPrimaryOutput
    , configChannelOutputPulseValue
    , configTimerOutputMode
    , configChannelOutputShadow
    , initChannelOcTimer
    , inclTimer
    ) where

import           Data.Function                 ((&))
import           Data.Maybe
import           Ivory.Language
import           Ivory.Language.Module
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

data TIMER_CHANNEL
    = TIMER_CH_0
    | TIMER_CH_1
    | TIMER_CH_2
    | TIMER_CH_3
    deriving (Show, Enum, Bounded)
instance ExtDef TIMER_CHANNEL Uint16

data TIMER_COMPARE_MODE
    = TIMER_OC_MODE_PWM0
    deriving (Show, Enum, Bounded)
instance ExtDef TIMER_COMPARE_MODE Uint16

data TIMER_COMPARE_SHADOW
    = TIMER_OC_SHADOW_DISABLE
    deriving (Show, Enum, Bounded)
instance ExtDef TIMER_COMPARE_SHADOW Uint16
--
data TIMER_CHANNEL_STATE
    = TIMER_CCX_ENABLE
    deriving (Show, Enum, Bounded)
instance ExtDef TIMER_CHANNEL_STATE Uint32

data TIMER_COMPL_CHANNEL_STATE
    = TIMER_CCXN_DISABLE
    deriving (Show, Enum, Bounded)
instance ExtDef TIMER_COMPL_CHANNEL_STATE Uint16

data TIMER_CHANNEL_POLARITY
    = TIMER_OC_POLARITY_HIGH
    deriving (Show, Enum, Bounded)
instance ExtDef TIMER_CHANNEL_POLARITY Uint16

data TIMER_COMPL_CHANNEL_POLARITY
    = TIMER_OCN_POLARITY_HIGH
    deriving (Show, Enum, Bounded)
instance ExtDef TIMER_COMPL_CHANNEL_POLARITY Uint16

data TIMER_CHANNEL_IDLE_STATE
    = TIMER_OC_IDLE_STATE_LOW
    deriving (Show, Enum, Bounded)
instance ExtDef TIMER_CHANNEL_IDLE_STATE Uint16

data TIMER_COMPL_CHANNEL_IDLE_STATE
    = TIMER_OCN_IDLE_STATE_LOW
    deriving (Show, Enum, Bounded)
instance ExtDef TIMER_COMPL_CHANNEL_IDLE_STATE Uint16


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
        { prescaler         :: Uint16
        ; alignedmode       :: Uint16
        ; counterdirection  :: Uint16
        ; clockdivision     :: Uint16
        ; period            :: Uint32
        ; repetitioncounter :: Uint8
        }
|]


data TIMER_OC_PARAM = TIMER_OC_PARAM
        { timerOutputState  :: TIMER_CHANNEL_STATE
        , timerOutputnState :: TIMER_COMPL_CHANNEL_STATE
        , timerOcPolarity   :: TIMER_CHANNEL_POLARITY
        , timerOcnPolarity  :: TIMER_COMPL_CHANNEL_POLARITY
        , timerOcIdleState  :: TIMER_CHANNEL_IDLE_STATE
        , timerOcnIdleState :: TIMER_COMPL_CHANNEL_IDLE_STATE
        }

[ivory|
    struct timer_oc_parameter_struct
        { outputstate       :: Uint32
        ; outputnstate      :: Uint16
        ; ocpolarity        :: Uint16
        ; ocnpolarity       :: Uint16
        ; ocidlestate       :: Uint16
        ; ocnidlestate      :: Uint16
        }
|]


inclTimer :: ModuleDef
inclTimer = do
    inclDef (def :: Cast TIMER_PERIPH Uint32)
    inclDef (def :: Cast TIMER_ALIGNE_MODE Uint16)
    inclDef (def :: Cast TIMER_COUNTER_DIRECTION Uint16)
    inclDef (def :: Cast TIMER_CLOCK_DIVISION Uint16)
    inclDef (def :: Cast TIMER_INT Uint32)
    inclDef (def :: Cast TIMER_INT_FLAG Uint32)
    inclDef (def :: Cast TIMER_CHANNEL Uint16)
    inclDef (def :: Cast TIMER_COMPARE_MODE Uint16)
    inclDef (def :: Cast TIMER_COMPARE_SHADOW Uint16)
    inclDef (def :: Cast TIMER_CHANNEL_STATE Uint32)
    inclDef (def :: Cast TIMER_COMPL_CHANNEL_STATE Uint16)
    inclDef (def :: Cast TIMER_CHANNEL_POLARITY Uint16)
    inclDef (def :: Cast TIMER_COMPL_CHANNEL_POLARITY Uint16)
    inclDef (def :: Cast TIMER_CHANNEL_IDLE_STATE Uint16)
    inclDef (def :: Cast TIMER_COMPL_CHANNEL_IDLE_STATE Uint16)
    incl timer_interrupt_flag_get
    incl timer_interrupt_flag_clear
    incl timer_interrupt_enable
    incl timer_deinit
    incl timer_enable
    incl timer_init
    incl timer_cnt
    incl timer_primary_output_config
    incl timer_channel_output_pulse_value_config
    incl timer_channel_output_mode_config
    incl timer_channel_output_shadow_config
    incl timer_auto_reload_shadow_enable


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


initTimer :: TIMER_PERIPH -> TIMER_PARAM -> Ivory (ProcEffects s ())    ()
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

timer_init :: Def ('[Uint32, Ref s (Struct "timer_parameter_struct")] :-> ())
timer_init = fun "timer_init"


readCounter :: TIMER_PERIPH -> Ivory eff Uint32
readCounter t = call timer_cnt (def t)

timer_cnt :: Def ('[Uint32] :-> Uint32)
timer_cnt = fun "TIMER_CNT"


configPrimaryOutput :: TIMER_PERIPH -> IBool -> Ivory eff ()
configPrimaryOutput t = call_ timer_primary_output_config (def t)

timer_primary_output_config :: Def ('[Uint32, IBool] :-> ())
timer_primary_output_config = fun "timer_primary_output_config"


timerParam :: TIMER_PARAM
timerParam =  TIMER_PARAM 0
                          TIMER_COUNTER_EDGE
                          TIMER_COUNTER_UP
                          TIMER_CKDIV_DIV1
                          65535
                          0


configChannelOutputPulseValue :: TIMER_PERIPH -> TIMER_CHANNEL -> Uint16 -> Ivory eff ()
configChannelOutputPulseValue t c = call_ timer_channel_output_pulse_value_config (def t) (def c)

timer_channel_output_pulse_value_config :: Def ('[Uint32, Uint16, Uint16] :-> ())
timer_channel_output_pulse_value_config = fun "timer_channel_output_pulse_value_config"


configTimerOutputMode :: TIMER_PERIPH -> TIMER_CHANNEL -> TIMER_COMPARE_MODE -> Ivory eff ()
configTimerOutputMode t ch c = call_ timer_channel_output_mode_config (def t) (def ch) (def c)

timer_channel_output_mode_config :: Def ('[Uint32, Uint16, Uint16] :-> ())
timer_channel_output_mode_config = fun "timer_channel_output_mode_config"


configChannelOutputShadow :: TIMER_PERIPH -> TIMER_CHANNEL -> TIMER_COMPARE_SHADOW -> Ivory eff ()
configChannelOutputShadow t ch s = call_ timer_channel_output_shadow_config (def t) (def ch) (def s)

timer_channel_output_shadow_config :: Def ('[Uint32, Uint16, Uint16] :-> ())
timer_channel_output_shadow_config = fun "timer_channel_output_shadow_config"


enableAutoReloadShadow :: TIMER_PERIPH -> Ivory eff ()
enableAutoReloadShadow t = call_ timer_auto_reload_shadow_enable (def t)

timer_auto_reload_shadow_enable :: Def ('[Uint32] :-> ())
timer_auto_reload_shadow_enable = fun "timer_auto_reload_shadow_enable"


initChannelOcTimer :: TIMER_PERIPH -> TIMER_CHANNEL -> TIMER_OC_PARAM -> Ivory (ProcEffects s ()) ()
initChannelOcTimer t ch p = do
    r <- local $ istruct
        [ outputstate   .= ival (p & timerOutputState  & def)
        , outputnstate  .= ival (p & timerOutputnState & def)
        , ocpolarity    .= ival (p & timerOcPolarity   & def)
        , ocnpolarity   .= ival (p & timerOcnPolarity  & def)
        , ocidlestate   .= ival (p & timerOcIdleState  & def)
        , ocnidlestate  .= ival (p & timerOcnIdleState & def)
        ]
    call_ timer_channel_output_config (def t) (def ch) r

timer_channel_output_config :: Def ('[ Uint32, Uint16, Ref s (Struct "timer_oc_parameter_struct")] :-> ())
timer_channel_output_config = fun "timer_channel_output_config"

timerOcParam :: TIMER_OC_PARAM
timerOcParam =  TIMER_OC_PARAM  TIMER_CCX_ENABLE
                                TIMER_CCXN_DISABLE
                                TIMER_OC_POLARITY_HIGH
                                TIMER_OCN_POLARITY_HIGH
                                TIMER_OC_IDLE_STATE_LOW
                                TIMER_OCN_IDLE_STATE_LOW
