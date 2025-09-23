{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Device.GD32F4xx.I2STRX where

import Control.Arrow (Arrow (arr))
import Control.Monad.State (MonadState)
import Core.Context
import Core.Handler
import Core.Task
import Data.Buffer
import Data.DoubleBuffer
import Data.Record
import Data.Value
import qualified Device.GD32F4xx.GPIO.Port as G
import Device.GD32F4xx.I2S
import GHC.Arr (array)
import GHC.TypeLits
import Interface.I2S
import qualified Interface.I2S as I
import qualified Interface.I2SRX as I
import qualified Interface.I2STX as I
import Ivory.Language
import Ivory.Stdlib as S
import Ivory.Support
import Support.Cast
import Support.Device.GD32F4xx.DMA
import Support.Device.GD32F4xx.GPIO
import Support.Device.GD32F4xx.IRQ
import Support.Device.GD32F4xx.Misc
import Support.Device.GD32F4xx.RCU
import Support.Device.GD32F4xx.SPI

data I2STRX tn rn = I2STRX
    { spi :: SPI_PERIPH
    , dmaPerTx :: DMA_PERIPH
    , dmaChTx :: DMA_CHANNEL
    , dmaParamsTx :: Record DMA_SINGLE_PARAM_STRUCT
    , dmaIRQnTx :: IRQn
    , txBuff :: DoubleBuffer tn Uint32
    , i2s_add :: SPI_PERIPH
    , dmaPerRx :: DMA_PERIPH
    , dmaChRx :: DMA_CHANNEL
    , dmaParamsRx :: Record DMA_SINGLE_PARAM_STRUCT
    , dmaIRQnRx :: IRQn
    , rxBuff :: DoubleBuffer rn Uint32
    , sample :: I.Sample
    }

mkI2STRX ::
    ( MonadState Context m
    , KnownNat tn
    , KnownNat rn
    ) =>
    SPI_PERIPH ->
    RCU_PERIPH ->
    RCU_PERIPH ->
    DMA_PERIPH ->
    DMA_CHANNEL ->
    DMA_SUBPERIPH ->
    IRQn ->
    (GPIO_PUPD -> G.Port) ->
    (GPIO_PUPD -> G.Port) ->
    (GPIO_PUPD -> G.Port) ->
    (GPIO_PUPD -> G.Port) ->
    SPI_PERIPH ->
    RCU_PERIPH ->
    DMA_PERIPH ->
    DMA_CHANNEL ->
    DMA_SUBPERIPH ->
    IRQn ->
    (GPIO_PUPD -> G.Port) ->
    m (I2STRX tn rn)
mkI2STRX
    spi
    rcuSpiTx
    rcuDmaTx
    dmaPerTx
    dmaChTx
    dmaSubPerTx
    dmaIRQnTx
    txPin
    wsPin
    sckPin
    mclkPin
    i2s_add
    rcuDmaRx
    dmaPerRx
    dmaChRx
    dmaSubPerRx
    dmaIRQnRx
    rxPin = do
        let dmaInitTx =
                dmaParam
                    [ periph_inc .= ival dma_periph_increase_disable
                    , memory_inc .= ival dma_memory_increase_enable
                    , periph_memory_width .= ival dma_periph_width_16bit
                    , circular_mode .= ival dma_circular_mode_disable
                    , direction .= ival dma_memory_to_periph
                    , priority .= ival dma_priority_ultra_high
                    ]
        dmaParamsTx <- record (symbol spi <> "_dma_param") dmaInitTx
        txBuff <- doubleBuffer $ symbol spi <> "_tx_buff"

        let tx = txPin gpio_pupd_none
        let ws = wsPin gpio_pupd_none
        let sck = sckPin gpio_pupd_none
        let mclk = mclkPin gpio_pupd_none

        G.initPort tx
        G.initPort ws
        G.initPort sck
        G.initPort mclk

        let dmaInitRx =
                dmaParam
                    [ periph_inc .= ival dma_periph_increase_disable
                    , memory_inc .= ival dma_memory_increase_enable
                    , periph_memory_width .= ival dma_periph_width_16bit
                    , circular_mode .= ival dma_circular_mode_disable
                    , direction .= ival dma_periph_to_memory
                    , priority .= ival dma_priority_high
                    ]
        dmaParamsRx <- record (symbol i2s_add <> "_dma_param") dmaInitRx
        rxBuff <- doubleBuffer (symbol i2s_add <> "_rx_buff")
        sample <- record (symbol i2s_add <> "_sample") [I.left .= izero, I.right .= izero]

        let rx = rxPin gpio_pupd_none

        G.initPort rx

        addStruct (Proxy :: Proxy I.SampleStruct)

        let i2strx = I2STRX{..}

        addInit (symbol spi) $ do
            enablePeriphClock rcuDmaTx
            enablePeriphClock rcuSpiTx
            deinitDMA dmaPerTx dmaChTx
            store (dmaParamsTx ~> periph_addr) =<< dataSPI spi
            store (dmaParamsTx ~> number) $ lengthDoubleArray txBuff * 2
            selectChannelSubperipheralDMA dmaPerTx dmaChTx dmaSubPerTx
            initI2S spi i2s_mode_mastertx i2s_std_phillips i2s_ckpl_low
            configPscI2S spi i2s_audiosample_48k i2s_frameformat_dt32b_ch32b i2s_mckout_enable
            enableIrqNvic dmaIRQnTx 1 0
            transmitBuff i2strx txBuff

            enablePeriphClock rcuDmaRx
            store (dmaParamsRx ~> periph_addr) =<< dataSPI i2s_add
            store (dmaParamsRx ~> number) $ lengthDoubleArray rxBuff * 2
            selectChannelSubperipheralDMA dmaPerRx dmaChRx dmaSubPerRx
            configFullDuplexModeI2S i2s_add i2s_mode_mastertx i2s_std_phillips i2s_ckpl_low i2s_frameformat_dt32b_ch32b
            enableIrqNvic dmaIRQnRx 1 0
            receiveBuff i2strx rxBuff

            enableI2S spi
            enableI2S i2s_add

        pure i2strx

instance
    (KnownNat tn) =>
    Handler I.HandleI2STX (I2STRX tn rn)
    where
    addHandler I.HandleI2STX{..} = do
        addModule $
            makeIRQHandler
                (dmaIRQnTx i2s)
                (handleDMATx i2s)
        addTask $ yeld "i2s_prepare_tx_buffer" $ do
            selectHandlerBuff
                (txBuff i2s)
                (prepareBuff handle)

handleDMATx :: (KnownNat tn) => I2STRX tn rn -> Ivory (ProcEffects s ()) ()
handleDMATx i2s = do
    handleI2S (dmaPerTx i2s) (dmaChTx i2s) dma_int_flag_ftf $ do
        transmitBuff i2s (txBuff i2s)

transmitBuff ::
    (KnownNat tn) =>
    I2STRX tn rn ->
    DoubleBuffer tn Uint32 ->
    Ivory (ProcEffects s ()) ()
transmitBuff i2s dbuff = do
    let callbackBuff =
            dataExchangeDmaI2S
                spi_dma_transmit
                (spi i2s)
                (dmaParamsTx i2s)
                (dmaPerTx i2s)
                (dmaChTx i2s)
                dma_chxctl_ftfie
    selectDmaBuff dbuff callbackBuff

prepareBuff ::
    (KnownNat n) =>
    Ivory (AllowBreak (ProcEffects s ())) I.Sample ->
    Buffer n Uint32 ->
    Ivory (ProcEffects s ()) ()
prepareBuff prepare buff = do
    i <- local $ ival (0 :: Uint16)
    forever $ do
        i' <- deref i
        when (i' >=? arrayLen buff) breakOut
        t <- prepare
        store (buff ! toIx i') . convert =<< deref (t ~> I.left)
        store (buff ! toIx (i' + 1)) . convert =<< deref (t ~> I.right)
        store i (i' + 2)
  where
    convert = swap16bit . twosComplementRep

instance (KnownNat rn) => Handler I.HandleI2SRX (I2STRX tn rn) where
    addHandler I.HandleI2SRX{..} = do
        addModule $
            makeIRQHandler
                (dmaIRQnRx i2s)
                (handleDMARx i2s)
        addTask $ yeld "i2s_process_rx_buffer" $ do
            selectHandlerBuff
                (rxBuff i2s)
                (processBuff i2s handle)

handleDMARx ::
    (KnownNat rn) =>
    I2STRX tn rn ->
    Ivory (ProcEffects s ()) ()
handleDMARx i2s = do
    handleI2S (dmaPerRx i2s) (dmaChRx i2s) dma_int_flag_ftf $ do
        receiveBuff i2s (rxBuff i2s)

receiveBuff ::
    (KnownNat rn) =>
    I2STRX tn rn ->
    DoubleBuffer rn Uint32 ->
    Ivory (ProcEffects s ()) ()
receiveBuff i2s dbuff = do
    let callbackBuff =
            dataExchangeDmaI2S
                spi_dma_receive
                (i2s_add i2s)
                (dmaParamsRx i2s)
                (dmaPerRx i2s)
                (dmaChRx i2s)
                dma_chxctl_ftfie
    selectDmaBuff dbuff callbackBuff

processBuff ::
    (KnownNat rn) =>
    I2STRX tn rn ->
    (forall eff. I.Sample -> Ivory eff ()) ->
    Buffer rn Uint32 ->
    Ivory (ProcEffects s ()) ()
processBuff i2s handle buff = do
    i <- local $ ival (0 :: Uint16)
    forever $ do
        i' <- deref i
        when (i' >=? arrayLen buff) breakOut
        store (sample i2s ~> I.left) . convert =<< deref (buff ! toIx i')
        store (sample i2s ~> I.right) . convert =<< deref (buff ! toIx (i' + 1))
        store i (i' + 2)
        handle (sample i2s)
  where
    convert = twosComplementCast . swap16bit

swap16bit w = (w `iShiftL` 16) .| (w `iShiftR` 16)
