module Device.GD32F3x0.ADC where

import Control.Monad.State (MonadState)
import Core.Context
import Device.GD32F3x0.GPIO.Port
import Interface.ADC qualified as I
import Ivory.Language
import Ivory.Stdlib
import Support.Device.GD32F3x0.ADC
import Support.Device.GD32F3x0.GPIO (GPIO_PUPD, gpio_pupd_none)
import Support.Device.GD32F3x0.RCU

newtype ADC = ADC {channel :: Uint8}

mkADC :: (MonadState Context m) => (GPIO_PUPD -> Port) -> Uint8 -> m ADC
mkADC mkPin channel = do
    addInit "adc" initADC
    initPort (mkPin gpio_pupd_none)
    pure ADC{channel}

initADC :: Ivory eff ()
initADC = do
    enablePeriphClock rcu_adc
    configClockADC rcu_adcck_apb2_div2
    configDataAlignmentADC adc_dataalign_right
    configChannelLengthADC adc_regular_channel 1
    configExternalTriggerSourceADC adc_regular_channel adc_exttrig_regular_none
    configExternalTriggerADC adc_regular_channel true
    enableADC
    enableCalibrationADC

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
