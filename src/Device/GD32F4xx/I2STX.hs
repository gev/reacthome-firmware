{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE RecordWildCards       #-}


module Device.GD32F4xx.I2STX where


import           Control.Monad.State          (MonadState)
import           Core.Context
import           Core.Handler
import           Core.Task
import           Data.Buffer
import           Data.Record
import           Data.Value
import qualified Device.GD32F4xx.GPIO.Port    as G
import           GHC.TypeLits
import qualified Interface.I2S                as I
import qualified Interface.I2STX              as I
import           Ivory.Language
import           Ivory.Stdlib                 as S
import           Ivory.Support
import           Support.Cast
import           Support.CMSIS.CoreCMFunc     (disableIRQ, enableIRQ)
import           Support.Device.GD32F4xx.DMA
import           Support.Device.GD32F4xx.GPIO
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.Misc
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.SPI
import           Device.GD32F4xx.I2S
import           Data.DoubleBuffer



data I2STX n = I2STX  { spi       :: SPI_PERIPH
                      , dmaPer    :: DMA_PERIPH
                      , dmaCh     :: DMA_CHANNEL
                      , dmaParams :: Record DMA_SINGLE_PARAM_STRUCT
                      , dmaIRQn   :: IRQn
                      , txBuff    :: DoubleBuffer n Uint32   
                      }



mkI2STX :: (MonadState Context m, KnownNat n)   => SPI_PERIPH
                                    -> RCU_PERIPH
                                    -> RCU_PERIPH
                                    -> DMA_PERIPH
                                    -> DMA_CHANNEL
                                    -> DMA_SUBPERIPH
                                    -> IRQn
                                    -> (GPIO_PUPD -> G.Port)
                                    -> (GPIO_PUPD -> G.Port)
                                    -> (GPIO_PUPD -> G.Port)
                                    -> (GPIO_PUPD -> G.Port) -> m (I2STX n)
mkI2STX spi rcuSpi rcuDma dmaPer dmaCh dmaSubPer dmaIRQn txPin wsPin sckPin mclkPin = do

    let dmaInit = dmaParam [ periph_inc          .= ival dma_periph_increase_disable
                           , memory_inc          .= ival dma_memory_increase_enable
                           , periph_memory_width .= ival dma_periph_width_16bit
                           , circular_mode       .= ival dma_circular_mode_disable
                           , direction           .= ival dma_memory_to_periph
                           , priority            .= ival dma_priority_ultra_high
                           ]
    dmaParams   <- record (symbol spi <> "_dma_param") dmaInit
    txBuff      <- doubleBuffer $ symbol spi <> "_tx_buff"

    let tx   = txPin   gpio_pupd_none
    let ws   = wsPin   gpio_pupd_none
    let sck  = sckPin  gpio_pupd_none
    let mclk = mclkPin gpio_pupd_none

    G.initPort tx
    G.initPort ws
    G.initPort sck
    G.initPort mclk

    addStruct (Proxy:: Proxy I.SampleStruct)

    addInit (symbol spi) $ do
            enablePeriphClock   rcuDma
            enablePeriphClock   rcuSpi
            deinitDMA           dmaPer dmaCh
            store (dmaParams ~> periph_addr) =<< dataSPI spi
            store (dmaParams ~> memory0_addr) =<< castArrayUint32ToUint32 (exchangeDBuff txBuff toCArray)
            store (dmaParams ~> number) $ lengthDoubleArray txBuff * 2
            initSingleDMA       dmaPer dmaCh dmaParams
            selectChannelSubperipheralDMA dmaPer dmaCh dmaSubPer
            initI2S             spi i2s_mode_mastertx i2s_std_phillips i2s_ckpl_low
            configPscI2S        spi i2s_audiosample_48k i2s_frameformat_dt32b_ch32b i2s_mckout_enable
            enableI2S           spi
            enableChannelDMA    dmaPer dmaCh
            enableSpiDma        spi spi_dma_transmit
            enableIrqNvic       dmaIRQn 1 0
            enableInterruptDMA  dmaPer dmaCh dma_chxctl_ftfie

    pure I2STX {spi, dmaPer, dmaCh, dmaParams, dmaIRQn, txBuff}

instance KnownNat n => Handler I.HandleI2STX (I2STX n) where
    addHandler I.HandleI2STX{..} = do
        addModule $ makeIRQHandler (dmaIRQn i2s) (handleDMA i2s)
        addTask $ yeld "i2s_prepare_buffer" $ do
            preBuff (txBuff i2s) (prepareBuff handle)

handleDMA :: KnownNat n => I2STX n -> Ivory (ProcEffects s ()) ()
handleDMA i2s = do
    handleI2S (dmaPer i2s) (dmaCh i2s) dma_int_flag_ftf $ do
        transmitBuff i2s (txBuff i2s)

transmitBuff :: KnownNat n => I2STX n -> DoubleBuffer n Uint32 -> Ivory (ProcEffects s ()) ()
transmitBuff i2s dbuff = do
    let handleBuff = dataExchangeDmaI2S spi_dma_transmit (spi i2s) (dmaParams i2s) (dmaPer i2s) (dmaCh i2s) dma_chxctl_ftfie
    exchangeDBuff dbuff handleBuff

prepareBuff :: KnownNat n => Ivory (AllowBreak (ProcEffects s ())) I.Sample -> Buffer n Uint32 -> Ivory (ProcEffects s ()) ()
prepareBuff prepare buff = do
    i <- local $ ival (0 :: Uint16)
    forever $ do
        i' <- deref i
        when (i' >=? arrayLen buff) breakOut
        t <- prepare
        store (buff ! toIx i') . swap16bit =<< deref (t ~> I.left)
        store (buff ! toIx (i' + 1)) . swap16bit =<< deref (t ~> I.right)
        store i (i' + 2)
    where swap16bit w = ( w `iShiftL` 16) .| ( w `iShiftR` 16)
