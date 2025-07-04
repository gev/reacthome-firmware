{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Device.GD32F4xx.UART where

import qualified Control.Monad                 as M
import           Control.Monad.State           (MonadState)
import           Core.Context
import           Core.Handler
import           Data.Buffer
import           Data.Concurrent.Queue         as Q
import           Data.Foldable
import           Data.Maybe
import           Data.Queue                    as Q
import           Data.Record
import qualified Device.GD32F4xx.GPIO.Port     as G
import           GHC.TypeNats
import           Interface.UART                (HandleUART (onDrain))
import qualified Interface.UART                as I
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Support
import           Ivory.Support.Device.GD32F4xx
import           Support.Cast
import           Support.Device.GD32F4xx.DMA
import           Support.Device.GD32F4xx.GPIO
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.Misc
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.USART as S



data UART rn tn = UART
    { uart      :: USART_PERIPH
    , rcu       :: RCU_PERIPH
    , uartIRQ   :: IRQn
    , dmaRcu    :: RCU_PERIPH
    , dmaPer    :: DMA_PERIPH
    , dmaCh     :: DMA_CHANNEL
    , dmaSubPer :: DMA_SUBPERIPH
    , dmaIRQn   :: IRQn
    , dmaParams :: Record DMA_SINGLE_PARAM_STRUCT
    , rxQueue   :: Queue  rn (Buffer rn Uint16)
    , txBuff    :: Buffer tn Uint16
    }



mkUART :: (MonadState Context m, KnownNat rn, KnownNat tn)
       => USART_PERIPH
       -> RCU_PERIPH
       -> IRQn
       -> RCU_PERIPH
       -> DMA_PERIPH
       -> DMA_CHANNEL
       -> DMA_SUBPERIPH
       -> IRQn
       -> (GPIO_PUPD -> G.Port)
       -> (GPIO_PUPD -> G.Port)
       -> m (UART rn tn)
mkUART uart rcu uartIRQ dmaRcu dmaPer dmaCh dmaSubPer dmaIRQn rx' tx' = do

    let dmaInit = dmaParam [ periph_inc          .= ival dma_periph_increase_disable
                           , memory_inc          .= ival dma_memory_increase_enable
                           , periph_memory_width .= ival dma_periph_width_16bit
                           , circular_mode       .= ival dma_circular_mode_disable
                           , direction           .= ival dma_memory_to_periph
                           , priority            .= ival dma_priority_ultra_high
                           ]
    dmaParams  <- record (symbol uart <> "_dma_param") dmaInit
    rxQueue    <- queue  (symbol uart <> "_rx_queue") =<< buffer (symbol uart <> "_rx_buff")
    txBuff     <- buffer (symbol uart <> "_tx_buff")

    let rx = rx' gpio_pupd_none
    let tx = tx' gpio_pupd_none

    G.initPort rx
    G.initPort tx

    addInit (symbol uart) $ do
            store (dmaParams ~> periph_addr) =<< udata uart
            enablePeriphClock   dmaRcu
            enableIrqNvic       uartIRQ 0 0
            enableIrqNvic       dmaIRQn 1 0
            enablePeriphClock   rcu

    pure UART { uart, rcu, uartIRQ, dmaRcu, dmaPer, dmaCh, dmaSubPer, dmaIRQn, dmaParams, rxQueue, txBuff }



instance KnownNat rn => Handler I.HandleUART (UART rn tn) where
    addHandler (I.HandleUART u@UART{..} onReceive onTransmit onDrain onError) = do
        addModule $ makeIRQHandler uartIRQ (handleUART u onReceive onDrain onError)
        addModule $ makeIRQHandler dmaIRQn (handleDMA dmaPer dmaCh uart onTransmit onDrain)


handleDMA :: DMA_PERIPH -> DMA_CHANNEL -> USART_PERIPH
          -> Ivory eff () -> Maybe (Ivory eff ()) -> Ivory eff ()
handleDMA dmaPer dmaCh uart onTransmit onDrain = do
    f <- getInterruptFlagDMA    dmaPer dmaCh dma_int_flag_ftf
    when f $ do
        M.when (isJust onDrain) $ do
            enableInterrupt     uart usart_int_tc
        onTransmit
        clearInterruptFlagDMA   dmaPer dmaCh dma_int_flag_ftf


handleUART :: KnownNat rn
           => UART rn tn
           -> Ivory eff ()
           -> Maybe (Ivory eff ())
           -> Ivory eff ()
           -> Ivory eff ()
handleUART u@UART{..} onReceive onDrain onError = do
    handleError u onError
    handleReceive u onReceive
    traverse_ (handleDrain uart) onDrain

handleError :: KnownNat rn => UART rn tn -> Ivory eff () -> Ivory eff ()
handleError UART{..} onError = do
    errs <- sequence [ clear usart_int_flag_err_ferr   [usart_flag_ferr]
                     , clear usart_int_flag_err_nerr   [usart_flag_nerr]
                     , clear usart_int_flag_err_orerr  [usart_flag_orerr]
                     , clear usart_int_flag_perr       [usart_flag_perr, usart_flag_eperr]
                     , clear usart_int_flag_rbne_orerr [usart_flag_orerr]
                     ]
    let err = foldr (.||) false errs
    when err onError
    where clear i f = do
            i' <- getInterruptFlag uart i
            when i' $ do
                mapM_ (clearFlag uart) f
                clearInterruptFlag uart i
            pure i'

handleReceive :: KnownNat rn => UART rn tn -> Ivory eff () -> Ivory eff ()
handleReceive UART{..} onReceive = do
    rbne  <- getInterruptFlag  uart usart_int_flag_rbne
    when rbne $ do
        push rxQueue $ \rxBuff ix -> do
            store (rxBuff ! ix) =<< S.receiveData uart
            onReceive
        clearInterruptFlag     uart usart_int_flag_rbne

handleDrain :: USART_PERIPH -> Ivory eff () -> Ivory eff ()
handleDrain uart onDrain = do
    tc <- getInterruptFlag uart usart_int_flag_tc
    when tc $ do
        disableInterrupt   uart usart_int_tc
        onDrain
        clearInterruptFlag uart usart_int_flag_tc



instance (KnownNat rn, KnownNat tn) => I.UART (UART rn tn) where
    configUART (UART {..}) baudrate length stop parity = do
        deinitUSART     uart
        configReceive   uart usart_receive_enable
        configTransmit  uart usart_transmit_enable
        enableInterrupt uart usart_int_rbne
        enableInterrupt uart usart_int_err
        enableInterrupt uart usart_int_perr
        setBaudrate     uart baudrate
        setWordLength   uart $ coerceWordLength length
        setStopBit      uart $ coerceStopBit    stop
        configParity    uart $ coerceParity     parity
        enableUSART     uart

    clearRX UART{..} = Q.clearConcurrently rxQueue

    receive UART{..} read =
        popConcurrently rxQueue $ \rxBuff ix ->
            read =<< deref (rxBuff ! ix)

    transmit UART{..} write = do
        size <- local $ ival (0 :: Uint16)
        write $ \value -> do
            size' <- deref size
            store (txBuff ! toIx size') value
            store size $ size' + 1

        store (dmaParams ~> memory0_addr) =<< castArrayUint16ToUint32 (toCArray txBuff)
        store (dmaParams ~> number) . safeCast =<< deref size
        deinitDMA dmaPer dmaCh
        initSingleDMA dmaPer dmaCh dmaParams
        disableCirculationDMA dmaPer dmaCh
        selectChannelSubperipheralDMA dmaPer dmaCh dmaSubPer
        transmitDMA uart usart_dent_enable
        enableInterruptDMA dmaPer dmaCh dma_chxctl_ftfie
        enableChannelDMA dmaPer dmaCh

    enable u = enableUSART (uart u)



coerceWordLength :: I.WordLength -> USART_WORD_LENGTH
coerceWordLength I.WL_8b = usart_wl_8bit
coerceWordLength I.WL_9b = usart_wl_9bit

coerceStopBit :: I.StopBit -> USART_STOP_BIT
coerceStopBit I.SB_0_5b = usart_stb_0_5bit
coerceStopBit I.SB_1b   = usart_stb_1bit
coerceStopBit I.SB_1_5b = usart_stb_1_5bit
coerceStopBit I.SB_2b   = usart_stb_2bit

coerceParity :: I.Parity -> USART_PARITY_CFG
coerceParity I.None = usart_pm_none
coerceParity I.Even = usart_pm_even
coerceParity I.Odd  = usart_pm_odd



instance Show (UART rn tn) where
    show UART{..} = symbol uart
