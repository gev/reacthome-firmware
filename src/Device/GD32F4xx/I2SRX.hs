{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}


module Device.GD32F4xx.I2SRX where


import           Control.Monad.State          (MonadState)
import           Core.Context
import           Core.Handler
import           Data.Buffer
import           Data.Record
import           Data.Value
import qualified Device.GD32F4xx.GPIO.Port    as G
import           GHC.TypeLits
import qualified Interface.I2SRX              as I
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

data I2SRX n = I2SRX  { i2s_add   :: SPI_PERIPH
                      , dmaPer    :: DMA_PERIPH
                      , dmaCh     :: DMA_CHANNEL
                      , dmaParams :: Record DMA_SINGLE_PARAM_STRUCT
                      , dmaIRQn   :: IRQn
                      , numRxBuff :: Value Uint8
                      , rxBuff0   :: Buffer n Uint32
                      , rxBuff1   :: Buffer n Uint32
                      }

mkI2SRX :: (MonadState Context m, KnownNat n)
            => SPI_PERIPH
            -> RCU_PERIPH
            -> DMA_PERIPH
            -> DMA_CHANNEL
            -> DMA_SUBPERIPH
            -> IRQn
            -> (GPIO_PUPD -> G.Port) -> m (I2SRX n)
mkI2SRX i2s_add rcuDma dmaPer dmaCh dmaSubPer dmaIRQn rxPin = do
    let dmaInit = dmaParam [ periph_inc          .= ival dma_periph_increase_disable
                           , memory_inc          .= ival dma_memory_increase_enable
                           , periph_memory_width .= ival dma_periph_width_32bit
                           , circular_mode       .= ival dma_circular_mode_enable
                           , direction           .= ival dma_periph_to_memory
                           , priority            .= ival dma_priority_high
                           ]
    dmaParams  <- record (symbol i2s_add <> "_dma_param") dmaInit
    rxBuff0    <- buffer (symbol i2s_add <> "_rx_buff")
    rxBuff1    <- buffer (symbol i2s_add <> "_rx_buff")
    numRxBuff  <- value  (symbol i2s_add <> "_num_rx_buff") 0

    let rx   = rxPin gpio_pupd_none
    G.initPort rx

    addInit (symbol i2s_add) $ do
        store (dmaParams ~> periph_addr) =<< dataSPI i2s_add
        store (dmaParams ~> memory0_addr) =<< castArrayUint32ToUint32 (toCArray rxBuff0)
        enablePeriphClock   rcuDma
        configFullDuplexModeI2S i2s_add i2s_mode_mastertx i2s_std_phillips i2s_ckpl_low i2s_frameformat_dt32b_ch32b
        enableI2S           i2s_add
        enableChannelDMA    dmaPer dmaCh
        enableIrqNvic       dmaIRQn 1 0
        enableInterruptDMA  dmaPer dmaCh dma_chxctl_ftfie
        enableSpiDma        i2s_add spi_dma_receive

    pure I2SRX {i2s_add, dmaPer, dmaCh, dmaParams, dmaIRQn, numRxBuff, rxBuff0, rxBuff1}


instance KnownNat n => Handler I.HandleI2SRX (I2SRX n) where
    addHandler I.HandleI2SRX{..} = do
        addModule $ makeIRQHandler (dmaIRQn i2s) (handleDMA i2s handle)


handleDMA :: KnownNat n => I2SRX n -> (Uint32 -> Ivory (AllowBreak eff) ()) -> Ivory eff ()
handleDMA i2s handle = do
    f <- getInterruptFlagDMA (dmaPer i2s) (dmaCh i2s) dma_int_flag_ftf
    S.when f $ do
        clearInterruptFlagDMA (dmaPer i2s) (dmaCh i2s) dma_int_flag_ftf
        numBuff <- deref $ numRxBuff i2s
        ifte_ (numBuff ==? 0)
            (do
                receiveBuff i2s (rxBuff0 i2s) (rxBuff1 i2s) handle
                store (numRxBuff i2s) 1
            )
            (do
                receiveBuff i2s (rxBuff1 i2s) (rxBuff0 i2s) handle
                store (numRxBuff i2s) 0
            )



receiveBuff :: KnownNat n => I2SRX n -> Buffer n Uint32 -> Buffer n Uint32 -> (Uint32 -> Ivory (AllowBreak eff) ()) -> Ivory eff ()
receiveBuff i2s buff0 buff1 handle = do
    store (dmaParams i2s ~> memory0_addr) =<< castArrayUint32ToUint32 (toCArray buff0)
    arrayMap $ \ix -> do
        word <- deref (buff1 ! ix)
        handle  word
    -- where swap16bit w = ( w `iShiftL` 16) .| ( w `iShiftR` 16)
