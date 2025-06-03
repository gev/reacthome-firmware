{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE RecordWildCards       #-}


module Device.GD32F4xx.I2STRX where


import           Control.Monad.State          (MonadState)
import           Core.Context
import           Core.Handler
import           Data.Buffer
import           Data.Record
import           Data.Value
import qualified Device.GD32F4xx.GPIO.Port    as G
import           GHC.TypeLits
import qualified Interface.I2STX              as IT
import qualified Interface.I2SRX              as IR
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



data I2STRX tn rn = I2STRX { spi       :: SPI_PERIPH
                         , dmaPerTx    :: DMA_PERIPH
                         , dmaChTx     :: DMA_CHANNEL
                         , dmaParamsTx :: Record DMA_SINGLE_PARAM_STRUCT
                         , dmaIRQnTx   :: IRQn
                         , numTxBuff   :: Value Uint8
                         , txBuff0     :: Buffer tn Uint32
                         , txBuff1     :: Buffer tn Uint32
   
                         , i2s_add     :: SPI_PERIPH
                         , dmaPerRx    :: DMA_PERIPH
                         , dmaChRx     :: DMA_CHANNEL
                         , dmaParamsRx :: Record DMA_SINGLE_PARAM_STRUCT
                         , dmaIRQnRx   :: IRQn
                         , numRxBuff   :: Value Uint8
                         , rxBuff0     :: Buffer rn Uint32
                         , rxBuff1     :: Buffer rn Uint32
                         }



mkI2STRX :: (MonadState Context m, KnownNat tn, KnownNat rn)   => SPI_PERIPH
                                    -> RCU_PERIPH
                                    -> RCU_PERIPH
                                    -> DMA_PERIPH
                                    -> DMA_CHANNEL
                                    -> DMA_SUBPERIPH
                                    -> IRQn
                                    -> (GPIO_PUPD -> G.Port)
                                    -> (GPIO_PUPD -> G.Port)
                                    -> (GPIO_PUPD -> G.Port)
                                    -> (GPIO_PUPD -> G.Port) 
                                    -> SPI_PERIPH 
                                    -> RCU_PERIPH 
                                    -> DMA_PERIPH 
                                    -> DMA_CHANNEL 
                                    -> DMA_SUBPERIPH 
                                    -> IRQn 
                                    -> (GPIO_PUPD -> G.Port)
                                    -> m (I2STRX tn rn)
mkI2STRX spi rcuSpiTx rcuDmaTx dmaPerTx dmaChTx dmaSubPerTx dmaIRQnTx txPin wsPin sckPin mclkPin 
        i2s_add rcuDmaRx dmaPerRx dmaChRx dmaSubPerRx dmaIRQnRx rxPin = do

    let dmaInitTx = dmaParam [ periph_inc          .= ival dma_periph_increase_disable
                           , memory_inc          .= ival dma_memory_increase_enable
                           , periph_memory_width .= ival dma_periph_width_16bit
                           , circular_mode       .= ival dma_circular_mode_enable
                           , direction           .= ival dma_memory_to_periph
                           , priority            .= ival dma_priority_ultra_high
                           ]
    dmaParamsTx <- record (symbol spi <> "_dma_param") dmaInitTx
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


    let dmaInitRx = dmaParam [ periph_inc          .= ival dma_periph_increase_disable
                           , memory_inc          .= ival dma_memory_increase_enable
                           , periph_memory_width .= ival dma_periph_width_16bit
                           , circular_mode       .= ival dma_circular_mode_enable
                           , direction           .= ival dma_periph_to_memory
                           , priority            .= ival dma_priority_high
                           ]
    dmaParamsRx  <- record (symbol i2s_add <> "_dma_param") dmaInitRx
    rxBuff0    <- buffer (symbol i2s_add <> "_rx_buff")
    rxBuff1    <- buffer (symbol i2s_add <> "_rx_buff")
    numRxBuff  <- value  (symbol i2s_add <> "_num_rx_buff") 0

    let rx   = rxPin gpio_pupd_none

    G.initPort rx


    addInit (symbol spi) $ do
        enablePeriphClock   rcuDmaTx
        enablePeriphClock   rcuSpiTx
        deinitDMA           dmaPerTx dmaChTx
        store (dmaParamsTx ~> periph_addr) =<< dataSPI spi
        store (dmaParamsTx ~> memory0_addr) =<< castArrayUint32ToUint32 (toCArray txBuff1)
        store (dmaParamsTx ~> number) $ arrayLen txBuff0 * 2
        initSingleDMA       dmaPerTx dmaChTx dmaParamsTx
        selectChannelSubperipheralDMA dmaPerTx dmaChTx dmaSubPerTx
        initI2S             spi i2s_mode_mastertx i2s_std_phillips i2s_ckpl_low
        configPscI2S        spi i2s_audiosample_48k i2s_frameformat_dt32b_ch32b i2s_mckout_enable
        enableChannelDMA    dmaPerTx dmaChTx
        enableSpiDma        spi spi_dma_transmit
        enableIrqNvic       dmaIRQnTx 1 0
        enableInterruptDMA  dmaPerTx dmaChTx dma_chxctl_ftfie

        enablePeriphClock   rcuDmaRx
        store (dmaParamsRx ~> periph_addr) =<< dataSPI i2s_add
        store (dmaParamsRx ~> memory0_addr) =<< castArrayUint32ToUint32 (toCArray rxBuff0)
        store (dmaParamsRx ~> number) $ arrayLen rxBuff0 * 2
        initSingleDMA       dmaPerRx dmaChRx dmaParamsRx
        selectChannelSubperipheralDMA dmaPerRx dmaChRx dmaSubPerRx
        configFullDuplexModeI2S i2s_add i2s_mode_mastertx i2s_std_phillips i2s_ckpl_low i2s_frameformat_dt32b_ch32b
        enableChannelDMA    dmaPerRx dmaChRx
        enableSpiDma        i2s_add spi_dma_receive
        enableIrqNvic       dmaIRQnRx 1 0
        enableInterruptDMA  dmaPerRx dmaChRx dma_chxctl_ftfie
        
        enableI2S           spi
        enableI2S           i2s_add


    pure I2STRX {spi, dmaPerTx, dmaChTx, dmaParamsTx, dmaIRQnTx, numTxBuff, txBuff0, txBuff1, i2s_add, dmaPerRx, dmaChRx, dmaParamsRx, dmaIRQnRx, numRxBuff, rxBuff0, rxBuff1}


instance KnownNat tn => Handler IT.HandleI2STX (I2STRX tn rn) where
    addHandler IT.HandleI2STX{..} = do
        addModule $ makeIRQHandler (dmaIRQnTx i2s) (handleDMATx i2s handle)


handleDMATx :: KnownNat tn => I2STRX tn rn -> Ivory (AllowBreak eff) Uint32 -> Ivory eff ()
handleDMATx i2s transmit = do
    f <- getInterruptFlagDMA (dmaPerTx i2s) (dmaChTx i2s) dma_int_flag_ftf
    S.when f $ do
        clearInterruptFlagDMA (dmaPerTx i2s) (dmaChTx i2s) dma_int_flag_ftf
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

transmitBuff :: KnownNat tn => I2STRX tn rn -> Buffer tn Uint32 -> Buffer tn Uint32 -> Ivory (AllowBreak eff) Uint32 -> Ivory eff ()
transmitBuff i2s buff0 buff1 transmit = do
    store (dmaParamsTx i2s ~> memory0_addr) =<< castArrayUint32ToUint32 (toCArray buff0)
    enableSpiDma (spi i2s) spi_dma_transmit
    arrayMap $ \ix -> do
        tn <- transmit
        store (buff1 ! ix) $ swap16bit tn
    where swap16bit w = ( w `iShiftL` 16) .| ( w `iShiftR` 16)



instance KnownNat rn => Handler IR.HandleI2SRX (I2STRX tn rn) where
    addHandler IR.HandleI2SRX{..} = do
        addModule $ makeIRQHandler (dmaIRQnRx i2s) (handleDMARx i2s handle)


handleDMARx :: KnownNat rn => I2STRX tn rn -> (Uint32 -> Ivory (AllowBreak eff) ()) -> Ivory eff ()
handleDMARx i2s handle = do
    f <- getInterruptFlagDMA (dmaPerRx i2s) (dmaChRx i2s) dma_int_flag_ftf
    S.when f $ do
        clearInterruptFlagDMA (dmaPerRx i2s) (dmaChRx i2s) dma_int_flag_ftf
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

receiveBuff :: KnownNat rn => I2STRX tn rn -> Buffer rn Uint32 -> Buffer rn Uint32 -> (Uint32 -> Ivory (AllowBreak eff) ()) -> Ivory eff ()
receiveBuff i2s buff0 buff1 handle = do
    store (dmaParamsRx i2s ~> memory0_addr) =<< castArrayUint32ToUint32 (toCArray buff0)
    arrayMap $ \ix -> do
        word <- deref (buff1 ! ix)
        handle $ swap16bit word
    where swap16bit w = ( w `iShiftL` 16) .| ( w `iShiftR` 16)
