{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Device.GD32F3x0.ADC where

import Control.Monad.State (MonadState)
import Core.Context
import Data.Buffer
import Data.Record
import Device.GD32F3x0.GPIO.Port
import qualified Interface.ADC as I
import Ivory.Language
import Ivory.Stdlib
import Support.Cast
import Support.Device.GD32F3x0.ADC
import Support.Device.GD32F3x0.DMA
import Support.Device.GD32F3x0.GPIO hiding (mode, rcu)
import Support.Device.GD32F3x0.RCU

newtype ADC = ADC {channel :: Uint8}

mkADC :: (MonadState Context m) => (GPIO_PUPD -> Port) -> Uint8 -> m ADC
mkADC mkPin channel = do
    let adc = ADC{channel}
    addInit "adc" $ initADC adc
    initPort (mkPin gpio_pupd_none)
    pure adc

initADC :: ADC -> Ivory eff ()
initADC ADC{..} = do
    enablePeriphClock rcu_adc
    configClockADC rcu_adcck_apb2_div2
    configDataAlignmentADC adc_dataalign_right
    configChannelLengthADC adc_regular_channel 1
    configExternalTriggerSourceADC adc_regular_channel adc_exttrig_regular_none
    configExternalTriggerADC adc_regular_channel true
    enableADC
    enableCalibrationADC
    enableSoftwareTriggerADC adc_regular_channel

instance I.ADC ADC where
    getResolution = const 12

    getAnalog ADC{..} = do
        configRegularChannelADC 0 channel adc_sampletime_55point5
        enableSoftwareTriggerADC adc_regular_channel
        forever do
            flag <- getFlagADC adc_flag_eoc
            when flag breakOut
        clearFlagADC adc_flag_eoc
        readRegularDataADC
