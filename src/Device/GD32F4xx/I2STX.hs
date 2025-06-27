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
import Device.GD32F4xx.I2S



data I2STX n = I2STX  { spi       :: SPI_PERIPH
                      , dmaPer    :: DMA_PERIPH
                      , dmaCh     :: DMA_CHANNEL
                      , dmaParams :: Record DMA_SINGLE_PARAM_STRUCT
                      , dmaIRQn   :: IRQn
                      , numTxBuff :: Value Uint8
                      , numPrBuff :: Value Uint8
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
                           , circular_mode       .= ival dma_circular_mode_disable
                           , direction           .= ival dma_memory_to_periph
                           , priority            .= ival dma_priority_ultra_high
                           ]
    dmaParams   <- record (symbol spi <> "_dma_param") dmaInit
    numPrBuff   <- value  (symbol spi <> "_num_pr_buff") 0
    numTxBuff   <- value  (symbol spi <> "_num_tx_buff") 1
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

    addStruct (Proxy:: Proxy I.SampleStruct)

    addInit (symbol spi) $ do
            enablePeriphClock   rcuDma
            enablePeriphClock   rcuSpi
            deinitDMA           dmaPer dmaCh
            store (dmaParams ~> periph_addr) =<< dataSPI spi
            store (dmaParams ~> memory0_addr) =<< castArrayUint32ToUint32 (toCArray txBuff0)
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

    pure I2STX {spi, dmaPer, dmaCh, dmaParams, dmaIRQn, numPrBuff, numTxBuff, txBuff0, txBuff1}


instance KnownNat n => Handler I.HandleI2STX (I2STX n) where
    addHandler I.HandleI2STX{..} = do
        addModule $ makeIRQHandler (dmaIRQn i2s) (handleDMA i2s)
        addTask $ yeld "i2s_prepare_buffer" $ do
            numPrBuff' <- deref $ numPrBuff i2s
            numTxBuff' <- deref $ numTxBuff i2s
            when (numPrBuff' /=? numTxBuff') $ do
                ifte_ (numPrBuff' ==? 0)
                    (prepareBuff (txBuff0 i2s) handle)
                    (prepareBuff (txBuff1 i2s) handle)
                store (numPrBuff i2s) numTxBuff'


handleDMA :: KnownNat n => I2STX n -> Ivory (ProcEffects s ()) ()
handleDMA i2s = do
    handleI2S (dmaPer i2s) (dmaCh i2s) dma_int_flag_ftf $ do
        numTxBuff' <- deref $ numTxBuff i2s
        ifte_ (numTxBuff' ==? 0)
            (transmitBuff i2s (txBuff1 i2s))
            (transmitBuff i2s (txBuff0 i2s))
        store (numTxBuff i2s) $ 1 - numTxBuff'

transmitBuff :: KnownNat n => I2STX n -> Buffer n Uint32 -> Ivory (ProcEffects s ()) ()
transmitBuff i2s buff = do
    dataExchangeDmaI2S spi_dma_transmit (spi i2s) buff (dmaParams i2s) (dmaPer i2s) (dmaCh i2s) dma_chxctl_ftfie

prepareBuff :: KnownNat n => Buffer n Uint32 -> Ivory (AllowBreak (ProcEffects s ())) I.Sample -> Ivory (ProcEffects s ()) ()
prepareBuff buff prepare = do
    i <- local $ ival (0 :: Uint16)
    forever $ do
        i' <- deref i
        when (i' >=? arrayLen buff) breakOut
        t <- prepare
        store (buff ! toIx i') . swap16bit =<< deref (t ~> I.left)
        store (buff ! toIx (i' + 1)) . swap16bit =<< deref (t ~> I.right)
        store i (i' + 2)
    where swap16bit w = ( w `iShiftL` 16) .| ( w `iShiftR` 16)
