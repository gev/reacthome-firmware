{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Device.GD32F3x0.ADC where


import           Control.Monad.State          (MonadState)
import           Core.Context
import           Data.Buffer
import           Device.GD32F3x0.GPIO.Port
import           Ivory.Language
import           Support.Cast                 (castArrayUint16ToUint32)
import           Support.Device.GD32F3x0.ADC
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.GPIO hiding (mode, rcu)
import           Support.Device.GD32F3x0.RCU



bufADC :: Buffer 16 Uint16


data ADC = ADC { channelADC :: Uint8
               }

mkADC :: (MonadState Context m) => [(GPIO_PUPD -> Port) -> Uint8] -> m ADC
mkADC pin channelADC = do

    let pin' = pin gpio_pupd_none
    initPort pin'

    addInit "adc_" $ do
        enablePeriphClock rcu_adc
        enablePeriphClock rcu_dma
        configClockADC    rcu_adcck_apb2_div2

        let dmaInit = dmaParam [ direction    .= ival dma_peripheral_to_memory
                               , memory_inc   .= ival dma_memory_increase_disable
                               , memory_width .= ival dma_memory_width_16bit
                               , periph_inc   .= ival dma_periph_increase_disable
                               , periph_width .= ival dma_peripheral_width_16bit
                               , priority     .= ival dma_priority_ultra_high
                               , memory_addr  .= ival
                               , number       .= ival 16
                               ]

        deinitDMA dma_ch0
        initDMA dma_ch0 dmaInit
        enableCirculationDMA dma_ch0
        enableChannelDMA dma_ch0


        configSpecialFunctionADC        adc_continuous_mode true
        configSpecialFunctionADC        adc_scan_mode true
        configDataAlignmentADC          adc_dataalign_right
        configChannelLengthADC          adc_regular_channel 16
        configRegularChannelADC         0 0 adc_sampletime_55point5
        configRegularChannelADC         1 1 adc_sampletime_55point5
        configRegularChannelADC         2 2 adc_sampletime_55point5
        configRegularChannelADC         3 3 adc_sampletime_55point5
        configRegularChannelADC         4 4 adc_sampletime_55point5
        configRegularChannelADC         5 5 adc_sampletime_55point5
        configRegularChannelADC         6 6 adc_sampletime_55point5
        configRegularChannelADC         7 7 adc_sampletime_55point5
        configRegularChannelADC         8 8 adc_sampletime_55point5
        configExternalTriggerSourceADC  adc_regular_channel adc_exttrig_regular_none
        configExternalTriggerADC        adc_regular_channel true
        enableADC
        enableCalibrationADC
        enableDmaModeADC
        enableSoftwareTriggerADC        adc_regular_channel


    pure ADC {}
