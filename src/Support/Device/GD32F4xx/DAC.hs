{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F4xx.DAC (
    DAC_PERIPH,
    dac0,
    dac1,
    WAVE_MODE_DAC,
    dac_wave_disable,
    ALIGN_DAC,
    dac_align_12b_r,
    deinitDAC,
    disableTriggerDAC,
    configWaveModeDAC,
    enableDAC,
    enableOutputBufferDAC,
    disableOutputBufferDAC,
    setDataDAC,
    inclDAC,
) where

import Ivory.Language
import Ivory.Support.Device.GD32F3x0
import Ivory.Support

newtype DAC_PERIPH = DAC_PERIPH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
instance ExtSymbol DAC_PERIPH

dac0 = DAC_PERIPH $ ext "DAC0"
dac1 = DAC_PERIPH $ ext "DAC1"

newtype WAVE_MODE_DAC = WAVE_MODE_DAC Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dac_wave_disable = WAVE_MODE_DAC $ ext "DAC_WAVE_DISABLE"

newtype ALIGN_DAC = ALIGN_DAC Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

dac_align_12b_r = ALIGN_DAC $ ext "DAC_ALIGN_12B_R"

deinitDAC :: Ivory eff ()
deinitDAC = call_ dac_deinit

dac_deinit :: Def ('[] :-> ())
dac_deinit = fun "dac_deinit"

disableTriggerDAC :: DAC_PERIPH -> Ivory eff ()
disableTriggerDAC = call_ dac_trigger_disable

dac_trigger_disable :: Def ('[DAC_PERIPH] :-> ())
dac_trigger_disable = fun "dac_trigger_disable"

configWaveModeDAC :: DAC_PERIPH -> WAVE_MODE_DAC -> Ivory eff ()
configWaveModeDAC = call_ dac_wave_mode_config

dac_wave_mode_config :: Def ('[DAC_PERIPH, WAVE_MODE_DAC] :-> ())
dac_wave_mode_config = fun "dac_wave_mode_config"

enableDAC :: DAC_PERIPH -> Ivory eff ()
enableDAC = call_ dac_enable

dac_enable :: Def ('[DAC_PERIPH] :-> ())
dac_enable = fun "dac_enable"

enableOutputBufferDAC :: DAC_PERIPH -> Ivory eff ()
enableOutputBufferDAC = call_ dac_output_buffer_enable

dac_output_buffer_enable :: Def ('[DAC_PERIPH] :-> ())
dac_output_buffer_enable = fun "dac_output_buffer_enable"

disableOutputBufferDAC :: DAC_PERIPH -> Ivory eff ()
disableOutputBufferDAC = call_ dac_output_buffer_disable

dac_output_buffer_disable :: Def ('[DAC_PERIPH] :-> ())
dac_output_buffer_disable = fun "dac_output_buffer_disable"

setDataDAC :: DAC_PERIPH -> ALIGN_DAC -> Uint16 -> Ivory eff ()
setDataDAC = call_ dac_data_set

dac_data_set :: Def ('[DAC_PERIPH, ALIGN_DAC, Uint16] :-> ())
dac_data_set = fun "dac_data_set"

inclDAC :: ModuleDef
inclDAC = do
    inclSym dac0
    inclSym dac1
    inclSym dac_wave_disable
    inclSym dac_align_12b_r
    incl dac_deinit
    incl dac_trigger_disable
    incl dac_wave_mode_config
    incl dac_enable
    incl dac_output_buffer_enable
    incl dac_output_buffer_disable
    incl dac_data_set
