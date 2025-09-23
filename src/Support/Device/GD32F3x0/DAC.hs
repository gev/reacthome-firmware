{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Support.Device.GD32F3x0.DAC (
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

disableTriggerDAC :: Ivory eff ()
disableTriggerDAC = call_ dac_trigger_disable

dac_trigger_disable :: Def ('[] :-> ())
dac_trigger_disable = fun "dac_trigger_disable"

configWaveModeDAC :: WAVE_MODE_DAC -> Ivory eff ()
configWaveModeDAC = call_ dac_wave_mode_config

dac_wave_mode_config :: Def ('[WAVE_MODE_DAC] :-> ())
dac_wave_mode_config = fun "dac_wave_mode_config"

enableDAC :: Ivory eff ()
enableDAC = call_ dac_enable

dac_enable :: Def ('[] :-> ())
dac_enable = fun "dac_enable"

enableOutputBufferDAC :: Ivory eff ()
enableOutputBufferDAC = call_ dac_output_buffer_enable

dac_output_buffer_enable :: Def ('[] :-> ())
dac_output_buffer_enable = fun "dac_output_buffer_enable"

disableOutputBufferDAC :: Ivory eff ()
disableOutputBufferDAC = call_ dac_output_buffer_disable

dac_output_buffer_disable :: Def ('[] :-> ())
dac_output_buffer_disable = fun "dac_output_buffer_disable"

setDataDAC :: ALIGN_DAC -> Uint16 -> Ivory eff ()
setDataDAC = call_ dac_data_set

dac_data_set :: Def ('[ALIGN_DAC, Uint16] :-> ())
dac_data_set = fun "dac_data_set"

inclDAC :: ModuleDef
inclDAC = do
    inclSym dac_wave_disable
    inclSym dac_align_12b_r
    incl dac_deinit
    incl dac_trigger_disable
    incl dac_wave_mode_config
    incl dac_enable
    incl dac_output_buffer_enable
    incl dac_output_buffer_disable
    incl dac_data_set
