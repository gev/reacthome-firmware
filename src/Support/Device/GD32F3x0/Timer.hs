{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Support.Device.GD32F3x0.Timer
    ( TIMER_PARAM_STRUCT
    , TIMER_PARAM
    , prescaler
    , alignedmode
    , counterdirection
    , clockdivision
    , period
    , repetitioncounter
    , timerParam

    , TIMER_OC_PARAM_STRUCT
    , TIMER_OC_PARAM
    , outputstate
    , outputnstate
    , ocpolarity
    , ocnpolarity
    , ocidlestate
    , ocnidlestate
    , timerOcParam
    , timerOcDefaultParam

    , TIMER_PERIPH
    , timer0
    , timer1
    , timer2
    , timer14
    , timer15

    , TIMER_ALIGNE_MODE
    , timer_counter_edge

    , TIMER_COUNTER_DIRECTION
    , timer_counter_up
    , timer_counter_down

    , TIMER_CLOCK_DIVISION
    , timer_ckdiv_div1
    , timer_ckdiv_div2
    , timer_ckdiv_div4

    , TIMER_INT
    , timer_int_up

    , TIMER_INT_FLAG
    , timer_int_flag_up

    , TIMER_CHANNEL
    , timer_ch_0
    , timer_ch_1
    , timer_ch_2
    , timer_ch_3

    , TIMER_COMPARE_MODE
    , timer_oc_mode_pwm0
    , timer_oc_mode_pwm1
    , timer_oc_mode_high
    , timer_oc_mode_low


    , TIMER_COMPARE_SHADOW
    , timer_oc_shadow_disable
    , timer_oc_shadow_enable

    , TIMER_CHANNEL_STATE
    , timer_ccx_enable

    , TIMER_COMPL_CHANNEL_STATE
    , timer_ccxn_disable

    , TIMER_CHANNEL_POLARITY
    , timer_oc_polarity_high

    , TIMER_COMPL_CHANNEL_POLARITY
    , timer_ocn_polarity_high

    , TIMER_CHANNEL_IDLE_STATE
    , timer_oc_idle_state_low
    , timer_oc_idle_state_high

    , TIMER_COMPL_CHANNEL_IDLE_STATE
    , timer_ocn_idle_state_low

    , TIMER_DMA_SOURCE
    , timer_dma_upd

    , deinitTimer
    , enableTimer
    , enableTimerInterrupt
    , getTimerInterruptFlag
    , clearTimerInterruptFlag
    , initTimer
    , readCounter
    , writeCounter
    , configPrimaryOutput
    , configChannelOutputPulseValue
    , configTimerOutputMode
    , configChannelOutputShadow
    , initChannelOcTimer
    , enableTimerDMA
    , ch0cv
    , ch1cv
    , ch2cv
    , ch3cv

    , inclTimer
    ) where

import           Ivory.Language
import           Ivory.Support
import           Ivory.Support.Device.GD32F3x0



type TIMER_PARAM_STRUCT = "timer_parameter_struct"
type TIMER_PARAM s = Ref s (Struct TIMER_PARAM_STRUCT)

[ivory|
    struct timer_parameter_struct
        { prescaler         :: Stored Uint16
        ; alignedmode       :: Stored TIMER_ALIGNE_MODE
        ; counterdirection  :: Stored TIMER_COUNTER_DIRECTION
        ; clockdivision     :: Stored TIMER_CLOCK_DIVISION
        ; period            :: Stored Uint32
        ; repetitioncounter :: Stored Uint8
        }
|]

timerParam :: [InitStruct  TIMER_PARAM_STRUCT]
           -> Init (Struct TIMER_PARAM_STRUCT)
timerParam p =
    istruct $ p <+>
        [ prescaler         .= ival 0
        , alignedmode       .= ival timer_counter_edge
        , counterdirection  .= ival timer_counter_up
        , clockdivision     .= ival timer_ckdiv_div1
        , period            .= ival 65535
        , repetitioncounter .= ival 0
        ]



type TIMER_OC_PARAM_STRUCT = "timer_oc_parameter_struct"
type TIMER_OC_PARAM s = Ref s (Struct TIMER_OC_PARAM_STRUCT)

[ivory|
    struct timer_oc_parameter_struct
        { outputstate  :: Stored TIMER_CHANNEL_STATE
        ; outputnstate :: Stored TIMER_COMPL_CHANNEL_STATE
        ; ocpolarity   :: Stored TIMER_CHANNEL_POLARITY
        ; ocnpolarity  :: Stored TIMER_COMPL_CHANNEL_POLARITY
        ; ocidlestate  :: Stored TIMER_CHANNEL_IDLE_STATE
        ; ocnidlestate :: Stored TIMER_COMPL_CHANNEL_IDLE_STATE
        }
|]

timerOcParam :: [InitStruct  TIMER_OC_PARAM_STRUCT]
             -> Init (Struct TIMER_OC_PARAM_STRUCT)
timerOcParam p =
    istruct $ p <+> timerOcDefaultParam


timerOcDefaultParam =
    [ outputstate  .= ival timer_ccx_enable
    , outputnstate .= ival timer_ccxn_disable
    , ocpolarity   .= ival timer_oc_polarity_high
    , ocnpolarity  .= ival timer_ocn_polarity_high
    , ocidlestate  .= ival timer_oc_idle_state_high
    , ocnidlestate .= ival timer_ocn_idle_state_low

    ]


newtype TIMER_PERIPH = TIMER_PERIPH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
instance ExtSymbol TIMER_PERIPH

timer0 = TIMER_PERIPH $ ext "TIMER0"
timer1 = TIMER_PERIPH $ ext "TIMER1"
timer2 = TIMER_PERIPH $ ext "TIMER2"
timer14 = TIMER_PERIPH $ ext "TIMER14"
timer15 = TIMER_PERIPH $ ext "TIMER15"



newtype TIMER_ALIGNE_MODE = TIMER_ALIGNE_MODE Uint16
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

timer_counter_edge = TIMER_ALIGNE_MODE $ ext "TIMER_COUNTER_EDGE"



newtype TIMER_COUNTER_DIRECTION = TIMER_COUNTER_DIRECTION Uint16
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

timer_counter_up   = TIMER_COUNTER_DIRECTION $ ext "TIMER_COUNTER_UP"
timer_counter_down = TIMER_COUNTER_DIRECTION $ ext "TIMER_COUNTER_DOWN"



newtype TIMER_INT = TIMER_INT Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

timer_int_up = TIMER_INT $ ext "TIMER_INT_UP"



newtype TIMER_INT_FLAG = TIMER_INT_FLAG Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

timer_int_flag_up = TIMER_INT_FLAG $ ext "TIMER_INT_FLAG_UP"



newtype TIMER_CLOCK_DIVISION = TIMER_CLOCK_DIVISION Uint16
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

timer_ckdiv_div1 = TIMER_CLOCK_DIVISION $ ext "TIMER_CKDIV_DIV1"
timer_ckdiv_div2 = TIMER_CLOCK_DIVISION $ ext "TIMER_CKDIV_DIV2"
timer_ckdiv_div4 =  TIMER_CLOCK_DIVISION$ ext "TIMER_CKDIV_DIV4"



newtype TIMER_CHANNEL = TIMER_CHANNEL Uint16
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

timer_ch_0 = TIMER_CHANNEL $ ext "TIMER_CH_0"
timer_ch_1 = TIMER_CHANNEL $ ext "TIMER_CH_1"
timer_ch_2 = TIMER_CHANNEL $ ext "TIMER_CH_2"
timer_ch_3 = TIMER_CHANNEL $ ext "TIMER_CH_3"



newtype TIMER_COMPARE_MODE = TIMER_COMPARE_MODE Uint16
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

timer_oc_mode_pwm0 = TIMER_COMPARE_MODE $ ext "TIMER_OC_MODE_PWM0"
timer_oc_mode_pwm1 = TIMER_COMPARE_MODE $ ext "TIMER_OC_MODE_PWM1"
timer_oc_mode_high = TIMER_COMPARE_MODE $ ext "TIMER_OC_MODE_HIGH"
timer_oc_mode_low  = TIMER_COMPARE_MODE $ ext "TIMER_OC_MODE_LOW"



newtype TIMER_COMPARE_SHADOW = TIMER_COMPARE_SHADOW Uint16
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

timer_oc_shadow_disable = TIMER_COMPARE_SHADOW $ ext "TIMER_OC_SHADOW_DISABLE"
timer_oc_shadow_enable  = TIMER_COMPARE_SHADOW $ ext "TIMER_OC_SHADOW_ENABLE"



newtype TIMER_CHANNEL_STATE = TIMER_CHANNEL_STATE Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

timer_ccx_enable = TIMER_CHANNEL_STATE $ ext "TIMER_CCX_ENABLE"



newtype TIMER_COMPL_CHANNEL_STATE = TIMER_COMPL_CHANNEL_STATE Uint16
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

timer_ccxn_disable = TIMER_COMPL_CHANNEL_STATE $ ext "TIMER_CCXN_DISABLE"



newtype TIMER_CHANNEL_POLARITY = TIMER_CHANNEL_POLARITY  Uint16
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

timer_oc_polarity_high = TIMER_CHANNEL_POLARITY $ ext "TIMER_OC_POLARITY_HIGH"



newtype TIMER_COMPL_CHANNEL_POLARITY = TIMER_COMPL_CHANNEL_POLARITY Uint16
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

timer_ocn_polarity_high = TIMER_COMPL_CHANNEL_POLARITY $ ext "TIMER_OCN_POLARITY_HIGH"



newtype TIMER_CHANNEL_IDLE_STATE = TIMER_CHANNEL_IDLE_STATE Uint16
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

timer_oc_idle_state_low = TIMER_CHANNEL_IDLE_STATE $ ext "TIMER_OC_IDLE_STATE_LOW"
timer_oc_idle_state_high = TIMER_CHANNEL_IDLE_STATE $ ext "TIMER_OC_IDLE_STATE_HIGH"



newtype TIMER_COMPL_CHANNEL_IDLE_STATE = TIMER_COMPL_CHANNEL_IDLE_STATE Uint16
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

timer_ocn_idle_state_low = TIMER_COMPL_CHANNEL_IDLE_STATE $ ext "TIMER_OCN_IDLE_STATE_LOW"


newtype TIMER_DMA_SOURCE = TIMER_DMA_SOURCE Uint16
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

timer_dma_upd = TIMER_DMA_SOURCE $ ext "TIMER_DMA_UPD"




deinitTimer :: TIMER_PERIPH -> Ivory eff ()
deinitTimer = call_ timer_deinit

timer_deinit :: Def ('[TIMER_PERIPH] :-> ())
timer_deinit = fun "timer_deinit"


enableTimer :: TIMER_PERIPH -> Ivory eff ()
enableTimer = call_ timer_enable

timer_enable :: Def ('[TIMER_PERIPH] :-> ())
timer_enable = fun "timer_enable"


enableTimerInterrupt :: TIMER_PERIPH -> TIMER_INT -> Ivory eff ()
enableTimerInterrupt = call_ timer_interrupt_enable

timer_interrupt_enable :: Def ('[TIMER_PERIPH, TIMER_INT] :-> ())
timer_interrupt_enable = fun "timer_interrupt_enable"


getTimerInterruptFlag :: TIMER_PERIPH -> TIMER_INT_FLAG -> Ivory eff IBool
getTimerInterruptFlag = call timer_interrupt_flag_get

timer_interrupt_flag_get :: Def ('[TIMER_PERIPH, TIMER_INT_FLAG] :-> IBool)
timer_interrupt_flag_get = fun "timer_interrupt_flag_get"


clearTimerInterruptFlag :: TIMER_PERIPH -> TIMER_INT_FLAG -> Ivory eff ()
clearTimerInterruptFlag = call_ timer_interrupt_flag_clear

timer_interrupt_flag_clear :: Def ('[TIMER_PERIPH, TIMER_INT_FLAG] :-> ())
timer_interrupt_flag_clear = fun "timer_interrupt_flag_clear"


initTimer :: TIMER_PERIPH -> TIMER_PARAM s -> Ivory eff ()
initTimer = call_ timer_init

timer_init :: Def ('[TIMER_PERIPH, TIMER_PARAM s] :-> ())
timer_init = fun "timer_init"


readCounter :: TIMER_PERIPH -> Ivory eff Uint32
readCounter = call timer_cnt

timer_cnt :: Def ('[TIMER_PERIPH] :-> Uint32)
timer_cnt = fun "TIMER_CNT"


writeCounter :: TIMER_PERIPH -> Uint32 -> Ivory eff ()
writeCounter = call_ timer_counter_value_config

timer_counter_value_config :: Def ('[TIMER_PERIPH, Uint32] :-> ())
timer_counter_value_config = fun "timer_counter_value_config"


configPrimaryOutput :: TIMER_PERIPH -> IBool -> Ivory eff ()
configPrimaryOutput = call_ timer_primary_output_config

timer_primary_output_config :: Def ('[TIMER_PERIPH, IBool] :-> ())
timer_primary_output_config = fun "timer_primary_output_config"


configChannelOutputPulseValue :: TIMER_PERIPH -> TIMER_CHANNEL -> Uint16 -> Ivory eff ()
configChannelOutputPulseValue = call_ timer_channel_output_pulse_value_config

timer_channel_output_pulse_value_config :: Def ('[TIMER_PERIPH, TIMER_CHANNEL, Uint16] :-> ())
timer_channel_output_pulse_value_config = fun "timer_channel_output_pulse_value_config"


configTimerOutputMode :: TIMER_PERIPH -> TIMER_CHANNEL -> TIMER_COMPARE_MODE -> Ivory eff ()
configTimerOutputMode = call_ timer_channel_output_mode_config

timer_channel_output_mode_config :: Def ('[TIMER_PERIPH, TIMER_CHANNEL, TIMER_COMPARE_MODE] :-> ())
timer_channel_output_mode_config = fun "timer_channel_output_mode_config"


configChannelOutputShadow :: TIMER_PERIPH -> TIMER_CHANNEL -> TIMER_COMPARE_SHADOW -> Ivory eff ()
configChannelOutputShadow = call_ timer_channel_output_shadow_config

timer_channel_output_shadow_config :: Def ('[TIMER_PERIPH, TIMER_CHANNEL, TIMER_COMPARE_SHADOW] :-> ())
timer_channel_output_shadow_config = fun "timer_channel_output_shadow_config"


enableAutoReloadShadow :: TIMER_PERIPH -> Ivory eff ()
enableAutoReloadShadow = call_ timer_auto_reload_shadow_enable

timer_auto_reload_shadow_enable :: Def ('[TIMER_PERIPH] :-> ())
timer_auto_reload_shadow_enable = fun "timer_auto_reload_shadow_enable"


initChannelOcTimer :: TIMER_PERIPH -> TIMER_CHANNEL -> TIMER_OC_PARAM s -> Ivory eff ()
initChannelOcTimer = call_ timer_channel_output_config

timer_channel_output_config :: Def ('[ TIMER_PERIPH, TIMER_CHANNEL, TIMER_OC_PARAM s] :-> ())
timer_channel_output_config = fun "timer_channel_output_config"


enableTimerDMA :: TIMER_PERIPH -> TIMER_DMA_SOURCE -> Ivory eff ()
enableTimerDMA = call_ timer_dma_enable

timer_dma_enable :: Def ('[ TIMER_PERIPH, TIMER_DMA_SOURCE] :-> ())
timer_dma_enable = fun "timer_dma_enable"


ch0cv :: TIMER_PERIPH -> Ivory eff Uint32
ch0cv = call timer_ch0cv

timer_ch0cv :: Def ('[TIMER_PERIPH] :-> Uint32)
timer_ch0cv = fun "(uint32_t) &TIMER_CH0CV"


ch1cv :: TIMER_PERIPH -> Ivory eff Uint32
ch1cv = call timer_ch1cv

timer_ch1cv :: Def ('[TIMER_PERIPH] :-> Uint32)
timer_ch1cv = fun "(uint32_t) &TIMER_CH1CV"


ch2cv :: TIMER_PERIPH -> Ivory eff Uint32
ch2cv = call timer_ch2cv

timer_ch2cv :: Def ('[TIMER_PERIPH] :-> Uint32)
timer_ch2cv = fun "(uint32_t) &TIMER_CH2CV"


ch3cv :: TIMER_PERIPH -> Ivory eff Uint32
ch3cv = call timer_ch3cv

timer_ch3cv :: Def ('[TIMER_PERIPH] :-> Uint32)
timer_ch3cv = fun "(uint32_t) &TIMER_CH3CV"


inclTimer :: ModuleDef
inclTimer = do
    inclSym timer0
    inclSym timer1
    inclSym timer2
    inclSym timer14
    inclSym timer15

    inclSym timer_counter_edge
    inclSym timer_counter_up
    inclSym timer_counter_down

    inclSym timer_int_up

    inclSym timer_int_flag_up

    inclSym timer_ckdiv_div1
    inclSym timer_ckdiv_div2
    inclSym timer_ckdiv_div4

    inclSym timer_ch_0
    inclSym timer_ch_1
    inclSym timer_ch_2
    inclSym timer_ch_3

    inclSym timer_oc_mode_pwm0
    inclSym timer_oc_mode_pwm1
    inclSym timer_oc_mode_high
    inclSym timer_oc_mode_low

    inclSym timer_oc_shadow_disable
    inclSym timer_oc_shadow_enable

    inclSym timer_ccx_enable

    inclSym timer_ccxn_disable

    inclSym timer_oc_polarity_high

    inclSym timer_ocn_polarity_high

    inclSym timer_oc_idle_state_low
    inclSym timer_oc_idle_state_high

    inclSym timer_ocn_idle_state_low

    inclSym timer_dma_upd

    incl timer_interrupt_flag_get
    incl timer_interrupt_flag_clear
    incl timer_interrupt_enable
    incl timer_deinit
    incl timer_enable
    incl timer_init
    incl timer_cnt
    incl timer_counter_value_config
    incl timer_primary_output_config
    incl timer_channel_output_pulse_value_config
    incl timer_channel_output_mode_config
    incl timer_channel_output_shadow_config
    incl timer_auto_reload_shadow_enable
    incl timer_channel_output_config
    incl timer_dma_enable
    incl timer_ch0cv
    incl timer_ch1cv
    incl timer_ch2cv
    incl timer_ch3cv
