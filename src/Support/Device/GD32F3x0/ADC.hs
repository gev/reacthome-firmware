{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Support.Device.GD32F3x0.ADC
    ( SPECIAL_FUNCTION_ADC
    , adc_continuous_mode
    , adc_scan_mode

    , DATA_ALIGNMENT_ADC
    , adc_dataalign_right

    , CHANNEL_GROUP_ADC
    , adc_regular_channel

    , CHANNEL_ADC
    , adc_channel_1
    , adc_channel_2
    , adc_channel_3
    , adc_channel_4

    , SAMPLE_TIME_ADC
    , adc_sampletime_1point5

    , EXTERNAL_TRIGGER_ADC
    , adc_exttrig_regular_none

    , configSpecialFunctionADC
    , configDataAlignmentADC
    , configRegularChannelADC
    , configExternalTriggerSourceADC
    , configExternalTriggerADC
    , enableADC
    , enableCalibrationADC
    , enableDmaModeADC
    , enableSoftwareTriggerADC
    , rdataADC

    , inclADC
    ) where

import           Ivory.Language
import           Ivory.Support.Device.GD32F3x0



newtype SPECIAL_FUNCTION_ADC = SPECIAL_FUNCTION_ADC Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

adc_continuous_mode  = SPECIAL_FUNCTION_ADC $ ext "ADC_CONTINUOUS_MODE"
adc_scan_mode        = SPECIAL_FUNCTION_ADC $ ext "ADC_SCAN_MODE"


newtype DATA_ALIGNMENT_ADC = DATA_ALIGNMENT_ADC Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

adc_dataalign_right  = DATA_ALIGNMENT_ADC $ ext "ADC_DATAALIGN_RIGHT"


newtype CHANNEL_GROUP_ADC = CHANNEL_GROUP_ADC Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

adc_regular_channel  = CHANNEL_GROUP_ADC $ ext "ADC_REGULAR_CHANNEL"


newtype CHANNEL_ADC = CHANNEL_ADC Uint8
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

adc_channel_1 = CHANNEL_ADC $ ext "ADC_CHANNEL_1"
adc_channel_2 = CHANNEL_ADC $ ext "ADC_CHANNEL_2"
adc_channel_3 = CHANNEL_ADC $ ext "ADC_CHANNEL_3"
adc_channel_4 = CHANNEL_ADC $ ext "ADC_CHANNEL_4"


newtype SAMPLE_TIME_ADC = SAMPLE_TIME_ADC Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

adc_sampletime_1point5  = SAMPLE_TIME_ADC $ ext "ADC_SAMPLETIME_1POINT5"


newtype EXTERNAL_TRIGGER_ADC = EXTERNAL_TRIGGER_ADC Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

adc_exttrig_regular_none  = EXTERNAL_TRIGGER_ADC $ ext "ADC_EXTTRIG_REGULAR_NONE"


configSpecialFunctionADC :: SPECIAL_FUNCTION_ADC -> IBool -> Ivory eff ()
configSpecialFunctionADC = call_ adc_special_function_config

adc_special_function_config :: Def ('[SPECIAL_FUNCTION_ADC, IBool] :-> ())
adc_special_function_config = fun "adc_special_function_config"


configDataAlignmentADC :: DATA_ALIGNMENT_ADC -> Ivory eff ()
configDataAlignmentADC = call_ adc_data_alignment_config

adc_data_alignment_config :: Def ('[DATA_ALIGNMENT_ADC] :-> ())
adc_data_alignment_config = fun "adc_data_alignment_config"


configChannelLengthADC :: CHANNEL_GROUP_ADC -> Uint32 -> Ivory eff ()
configChannelLengthADC = call_ adc_channel_length_config

adc_channel_length_config :: Def ('[CHANNEL_GROUP_ADC, Uint32] :-> ())
adc_channel_length_config = fun "adc_channel_length_config"


configRegularChannelADC :: Uint8 -> CHANNEL_ADC -> SAMPLE_TIME_ADC -> Ivory eff ()
configRegularChannelADC = call_ adc_regular_channel_config

adc_regular_channel_config :: Def ('[Uint8, CHANNEL_ADC, SAMPLE_TIME_ADC] :-> ())
adc_regular_channel_config = fun "adc_regular_channel_config"


configExternalTriggerSourceADC :: CHANNEL_GROUP_ADC -> EXTERNAL_TRIGGER_ADC -> Ivory eff ()
configExternalTriggerSourceADC = call_ adc_external_trigger_source_config

adc_external_trigger_source_config :: Def ('[CHANNEL_GROUP_ADC, EXTERNAL_TRIGGER_ADC] :-> ())
adc_external_trigger_source_config = fun "adc_external_trigger_source_config"


configExternalTriggerADC :: CHANNEL_GROUP_ADC -> IBool -> Ivory eff ()
configExternalTriggerADC = call_ adc_external_trigger_config

adc_external_trigger_config :: Def ('[CHANNEL_GROUP_ADC, IBool] :-> ())
adc_external_trigger_config = fun "adc_external_trigger_config"


enableADC :: Ivory eff ()
enableADC = call_ adc_enable

adc_enable :: Def ('[] :-> ())
adc_enable = fun "adc_enable"


enableCalibrationADC :: Ivory eff ()
enableCalibrationADC = call_ adc_calibration_enable

adc_calibration_enable :: Def ('[] :-> ())
adc_calibration_enable = fun "adc_calibration_enable"


enableDmaModeADC :: Ivory eff ()
enableDmaModeADC = call_ adc_dma_mode_enable

adc_dma_mode_enable :: Def ('[] :-> ())
adc_dma_mode_enable = fun "adc_dma_mode_enable"


enableSoftwareTriggerADC :: CHANNEL_GROUP_ADC -> Ivory eff ()
enableSoftwareTriggerADC = call_ adc_software_trigger_enable

adc_software_trigger_enable :: Def ('[CHANNEL_GROUP_ADC] :-> ())
adc_software_trigger_enable = fun "adc_software_trigger_enable"


rdataADC :: Ivory eff Uint32
rdataADC = call adc_rdata

adc_rdata :: Def ('[] :-> Uint32)
adc_rdata = fun "(uint32_t) &ADC_RDATA"


inclADC :: ModuleDef
inclADC = do
    inclSym adc_continuous_mode
    inclSym adc_scan_mode

    inclSym adc_dataalign_right

    inclSym adc_regular_channel

    inclSym adc_channel_1
    inclSym adc_channel_2
    inclSym adc_channel_3
    inclSym adc_channel_4

    inclSym adc_sampletime_1point5

    inclSym adc_exttrig_regular_none

    incl adc_special_function_config
    incl adc_data_alignment_config
    incl adc_channel_length_config
    incl adc_regular_channel_config
    incl adc_external_trigger_source_config
    incl adc_external_trigger_config
    incl adc_enable
    incl adc_calibration_enable
    incl adc_dma_mode_enable
    incl adc_software_trigger_enable
    incl adc_rdata