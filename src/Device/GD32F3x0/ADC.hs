{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE RecordWildCards  #-}

module Device.GD32F3x0.ADC where


import           Control.Monad.State          (MonadState)
import           Core.Context
import           Data.Buffer
import           Data.Record
import           Device.GD32F3x0.GPIO.Port
import qualified Interface.ADC                as I
import           Ivory.Language
import           Support.Cast
import           Support.Device.GD32F3x0.ADC
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.GPIO hiding (mode, rcu)
import           Support.Device.GD32F3x0.RCU



data ADC = ADC
    { buff      :: Buffer 16 Uint16
    , dmaParams :: Record DMA_PARAM_STRUCT
    , channel   :: Uint8
    }



mkADC :: (MonadState Context m) => (GPIO_PUPD -> Port) -> Uint8 -> m ADC
mkADC mkPin channel = do
    initPort (mkPin gpio_pupd_none)
    buff <- buffer "adc"

    let dmaInit = dmaParam [ direction    .= ival dma_peripheral_to_memory
                           , memory_inc   .= ival dma_memory_increase_enable
                           , memory_width .= ival dma_memory_width_16bit
                           , periph_inc   .= ival dma_periph_increase_disable
                           , periph_width .= ival dma_peripheral_width_16bit
                           , priority     .= ival dma_priority_ultra_high
                           , number       .= ival 16
                           ]
    dmaParams <- record "dma0_dma_param" dmaInit

    let adc = ADC {buff, dmaParams, channel}
    addInit "adc" $ initADC adc
    pure adc



initADC :: ADC -> Ivory eff ()
initADC ADC{..} = do
    enablePeriphClock rcu_adc
    enablePeriphClock rcu_dma
    configClockADC    rcu_adcck_apb2_div2

    store (dmaParams ~> memory_addr) =<< castArrayUint16ToUint32 (toCArray buff)
    store (dmaParams ~> periph_addr)  adc_rdata
    deinitDMA dma_ch0
    initDMA dma_ch0 dmaParams
    enableCirculationDMA dma_ch0
    enableChannelDMA dma_ch0


    configSpecialFunctionADC         adc_continuous_mode true
    configSpecialFunctionADC         adc_scan_mode true
    configDataAlignmentADC           adc_dataalign_right
    configChannelLengthADC           adc_regular_channel 16
    configRegularChannelADC          0  0 adc_sampletime_55point5
    configRegularChannelADC          1  1 adc_sampletime_55point5
    configRegularChannelADC          2  2 adc_sampletime_55point5
    configRegularChannelADC          3  3 adc_sampletime_55point5
    configRegularChannelADC          4  4 adc_sampletime_55point5
    configRegularChannelADC          5  5 adc_sampletime_55point5
    configRegularChannelADC          6  6 adc_sampletime_55point5
    configRegularChannelADC          7  7 adc_sampletime_55point5
    configRegularChannelADC          8  8 adc_sampletime_55point5
    configRegularChannelADC          9  9 adc_sampletime_55point5
    configRegularChannelADC         10 10 adc_sampletime_55point5
    configRegularChannelADC         11 11 adc_sampletime_55point5
    configRegularChannelADC         12 12 adc_sampletime_55point5
    configRegularChannelADC         13 13 adc_sampletime_55point5
    configRegularChannelADC         14 14 adc_sampletime_55point5
    configRegularChannelADC         15 15 adc_sampletime_55point5
    configExternalTriggerSourceADC  adc_regular_channel adc_exttrig_regular_none
    configExternalTriggerADC        adc_regular_channel true
    enableADC
    enableCalibrationADC
    enableDmaModeADC
    enableSoftwareTriggerADC        adc_regular_channel


instance I.ADC ADC where
    getResolution = const 12
    getAnalog ADC{..} = deref $ buff ! toIx channel
