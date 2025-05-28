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
import           Data.Buffer
import           Data.Record
import           Data.Value
import qualified Device.GD32F4xx.GPIO.Port    as G
import           GHC.TypeLits
import qualified Interface.I2STX              as I
import           Ivory.Language
import           Ivory.Stdlib                 as S
import           Ivory.Support
import           Support.Cast
import           Support.Device.GD32F4xx.DMA
import           Support.Device.GD32F4xx.GPIO
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.Misc
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.SPI



data I2STX n = I2STX  { spi       :: SPI_PERIPH
                      , dmaPer    :: DMA_PERIPH
                      , dmaCh     :: DMA_CHANNEL
                      , dmaParams :: Record DMA_SINGLE_PARAM_STRUCT
                      , dmaIRQn   :: IRQn
                      , numTxBuff :: Value Uint8
                      , txBuff0   :: Buffer n Uint32
                      , txBuff1   :: Buffer n Uint32
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
                           , circular_mode       .= ival dma_circular_mode_enable
                           , direction           .= ival dma_memory_to_periph
                           , priority            .= ival dma_priority_ultra_high
                           ]
    dmaParams   <- record (symbol spi <> "_dma_param") dmaInit
    numTxBuff   <- value  (symbol spi <> "_num_tx_buff") 0
    txBuff0     <- buffer $ symbol spi <> "_tx_buff0"
    txBuff1     <- buffer $ symbol spi <> "_tx_buff1"

    let tx   = txPin   gpio_pupd_none
    let ws   = wsPin   gpio_pupd_none
    let sck  = sckPin  gpio_pupd_none
    let mclk = mclkPin gpio_pupd_none

    G.initPort tx
    G.initPort ws
    G.initPort sck
    G.initPort mclk

    addInit (symbol spi) $ do
            enablePeriphClock   rcuDma
            enablePeriphClock   rcuSpi
            deinitDMA           dmaPer dmaCh
            store (dmaParams ~> periph_addr) =<< dataSPI spi
            store (dmaParams ~> memory0_addr) =<< castArrayUint32ToUint32 (toCArray txBuff1)
            store (dmaParams ~> number) $ arrayLen txBuff0 * 2
            initSingleDMA       dmaPer dmaCh dmaParams
            selectChannelSubperipheralDMA dmaPer dmaCh dmaSubPer
            initI2S             spi i2s_mode_mastertx i2s_std_phillips i2s_ckpl_low
            configPscI2S        spi i2s_audiosample_48k i2s_frameformat_dt32b_ch32b i2s_mckout_enable
            enableI2S           spi
            enableChannelDMA    dmaPer dmaCh
            enableSpiDma        spi spi_dma_transmit
            enableIrqNvic       dmaIRQn 1 0
            enableInterruptDMA  dmaPer dmaCh dma_chxctl_ftfie


    pure I2STX {spi, dmaPer, dmaCh, dmaParams, dmaIRQn, numTxBuff, txBuff0, txBuff1}


instance KnownNat n => Handler I.HandleI2STX (I2STX n) where
    addHandler I.HandleI2STX{..} = do
        addModule $ makeIRQHandler (dmaIRQn i2s) (handleDMA i2s handle)


handleDMA :: KnownNat n => I2STX n -> Ivory (AllowBreak eff) Uint32 -> Ivory eff ()
handleDMA i2s transmit = do
    f <- getInterruptFlagDMA (dmaPer i2s) (dmaCh i2s) dma_int_flag_ftf
    S.when f $ do
        clearInterruptFlagDMA (dmaPer i2s) (dmaCh i2s) dma_int_flag_ftf
        numBuff <- deref $ numTxBuff i2s
        ifte_ (numBuff ==? 0)
            (do
                transmitBuff i2s (txBuff0 i2s) (txBuff1 i2s) transmit
                store (numTxBuff i2s) 1
            )
            (do
                transmitBuff i2s (txBuff1 i2s) (txBuff0 i2s) transmit
                store (numTxBuff i2s) 0
            )



transmitBuff :: KnownNat n => I2STX n -> Buffer n Uint32 -> Buffer n Uint32 -> Ivory (AllowBreak eff) Uint32 -> Ivory eff ()
transmitBuff i2s buff0 buff1 transmit = do
    store (dmaParams i2s ~> memory0_addr) =<< castArrayUint32ToUint32 (toCArray buff0)
    enableSpiDma (spi i2s) spi_dma_transmit
    arrayMap $ \ix -> do
        t <- transmit
        store (buff1 ! ix) $ swap16bit t
    where swap16bit w = ( w `iShiftL` 16) .| ( w `iShiftR` 16)
