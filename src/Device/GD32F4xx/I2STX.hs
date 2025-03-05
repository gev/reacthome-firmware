{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE RecordWildCards #-}


module Device.GD32F4xx.I2STX where


import               Support.Device.GD32F4xx.DMA
import               Support.Device.GD32F4xx.GPIO
import               Support.Device.GD32F4xx.IRQ
import               Support.Device.GD32F4xx.RCU
import               Support.Device.GD32F4xx.SPI
import               Ivory.Language
import               Data.Buffer
import               Core.Context
import               Control.Monad.RWS
import               qualified Device.GD32F4xx.GPIO.Port as G
import               Core.Handler
import               GHC.TypeLits
import               qualified Interface.I2STX as I
import               Ivory.Support
import               Data.Record
import               Support.Device.GD32F4xx.Misc
import               Data.Concurrent.Queue




data I2STX = I2STX  { spi       :: SPI_PERIPH
                    , dmaPer    :: DMA_PERIPH
                    , dmaCh     :: DMA_CHANNEL
                    , dmaIRQn   :: IRQn
                    , txQueue   :: Queue  4480
                    , txBuff    :: Buffer 4480 Uint32
                    , txBuffDMA :: Buffer  320 Uint32
                    }



mkI2STX :: (MonadState Context m)   => SPI_PERIPH
                                    -> RCU_PERIPH
                                    -> RCU_PERIPH
                                    -> DMA_PERIPH
                                    -> DMA_CHANNEL
                                    -> DMA_SUBPERIPH
                                    -> IRQn
                                    -> (GPIO_PUPD -> G.Port)
                                    -> (GPIO_PUPD -> G.Port)
                                    -> (GPIO_PUPD -> G.Port)
                                    -> (GPIO_PUPD -> G.Port) -> m I2STX
mkI2STX spi rcuSpi rcuDma dmaPer dmaCh dmaSubPer dmaIRQn txPin wsPin sckPin mclkPin = do

    let dmaInit = dmaParam [ periph_inc          .= ival dma_periph_increase_disable
                           , memory_inc          .= ival dma_memory_increase_enable
                           , periph_memory_width .= ival dma_periph_width_16bit
                           , circular_mode       .= ival dma_circular_mode_enable
                           , direction           .= ival dma_memory_to_periph
                           , priority            .= ival dma_priority_ultra_high
                           ]
    dmaParams   <- record (symbol spi <> "_dma_param") dmaInit
    txBuff      <- buffer (symbol spi <> "_tx_buff")
    txQueue     <- queue (symbol spi <> "_tx_queue")
    txBuffDMA   <- buffer (symbol spi <> "_tx_buff_dma")

    let tx   = txPin gpio_pupd_none
    let ws   = wsPin gpio_pupd_none
    let sck  = sckPin gpio_pupd_none
    let mclk = mclkPin gpio_pupd_none

    G.initPort tx
    G.initPort ws
    G.initPort sck
    G.initPort mclk

    addInit (symbol spi) $ do
            store (dmaParams ~> periph_addr) =<< dataSPI spi
            enablePeriphClock   rcuSpi
            enablePeriphClock   rcuDma
            initI2S             spi i2s_mode_mastertx i2s_std_phillips i2s_ckpl_low
            configPscI2S        spi i2s_audiosample_48k i2s_frameformat_dt32b_ch32b i2s_mckout_enable
            enableSpiDma        spi spi_dma_transmit
            enableIrqNvic       dmaIRQn 1 0
            enableInterruptDMA  dmaPer dmaCh dma_chxctl_ftfie

    pure I2STX {spi, dmaPer, dmaCh, dmaIRQn, txBuff, txQueue, txBuffDMA}


instance Handler I.HandleI2STX I2STX where
    addHandler I.HandleI2STX{..} = do
        addModule $ makeIRQHandler (dmaIRQn i2s) (handleDMA (dmaPer i2s) (dmaCh i2s) (txBuff i2s) (txQueue i2s) (txBuffDMA i2s))


handleDMA :: DMA_PERIPH -> DMA_CHANNEL -> Buffer 4480 Uint32 -> Queue  4480 -> Buffer  320 Uint32 -> Ivory eff ()
handleDMA dmaPer dmaCh txBuff txQueue txBuffDMA = do
    f <- getInterruptFlagDMA dmaPer dmaCh dma_int_flag_ftf
    when f $ do
        clearInterruptFlagDMA dmaPer dmaCh dma_int_flag_ftf
        sQueue' <- size txQueue 
        ifte_ ( safeCast sQueue' <? 320)
            (do
                arrayMap $ \ix -> store (txBuffDMA ! ix) 0 )
            (do 
                pure())


instance I.I2STX I2STX where
    transmit I2STX{..} write = do
            write $ \value -> do
                push txQueue $ \i ->
                    store (txBuff ! toIx i) value
