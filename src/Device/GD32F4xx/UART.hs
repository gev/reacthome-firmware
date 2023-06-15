{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F4xx.UART where

import qualified Control.Monad                 as M
import           Control.Monad.Writer          (MonadWriter)
import           Core.Context
import           Core.Handler
import           Data.Foldable
import           Data.Maybe
import           Data.Record
import qualified Device.GD32F4xx.GPIO          as G
import           Interface.UART                (HandleUART (onDrain))
import qualified Interface.UART                as I
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Support
import           Ivory.Support.Device.GD32F4xx
import           Support.Cast
import           Support.Device.GD32F4xx.DMA
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.Misc
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.USART as S


data UART = UART
    { uart      :: USART_PERIPH
    , rcu       :: RCU_PERIPH
    , uartIRQ   :: IRQn
    , dmaRcu    :: RCU_PERIPH
    , dmaPer    :: DMA_PERIPH
    , dmaCh     :: DMA_CHANNEL
    , dmaSubPer :: DMA_SUBPERIPH
    , dmaIRQn   :: IRQn
    , dmaParams :: Record DMA_SINGLE_PARAM_STRUCT
    , rx        :: G.Port
    , tx        :: G.Port
    }



mkUART :: MonadWriter Context m
       => USART_PERIPH
       -> RCU_PERIPH
       -> IRQn
       -> RCU_PERIPH
       -> DMA_PERIPH
       -> DMA_CHANNEL
       -> DMA_SUBPERIPH
       -> IRQn
       -> G.Port
       -> G.Port
       -> m UART
mkUART uart rcu uartIRQ dmaRcu dmaPer dmaCh dmaSubPer dmaIRQn rx tx = do

    let dmaInit = dmaParam [ periph_inc          .= ival dma_periph_increase_disable
                           , memory_inc          .= ival dma_memory_increase_enable
                           , periph_memory_width .= ival dma_periph_width_16bit
                           , circular_mode       .= ival dma_circular_mode_disable
                           , direction           .= ival dma_memory_to_periph
                           , priority            .= ival dma_priority_ultra_high
                           ]
    dmaParams  <- record (symbol uart <> "_dma_param") dmaInit

    let initUART' :: Def ('[] ':-> ())
        initUART' = proc (symbol uart <> "_init") $ body $ do
            store (dmaParams ~> periph_addr) =<< udata uart
            enablePeriphClock   dmaRcu
            enableIrqNvic       uartIRQ 0 0
            enableIrqNvic       dmaIRQn 1 0
            enablePeriphClock   rcu

    addInit $ G.initPort rx
    addInit $ G.initPort tx
    addInit initUART'

    pure UART { uart, rcu, uartIRQ, dmaRcu, dmaPer, dmaCh, dmaSubPer, dmaIRQn, dmaParams, rx, tx }



instance Handler I.HandleUART UART where
    addHandler (I.HandleUART UART{..} onReceive onTransmit onDrain) = do
        addModule $ makeIRQHandler uartIRQ (handleUART uart onReceive onDrain)
        addModule $ makeIRQHandler dmaIRQn (handleDMA dmaPer dmaCh uart onTransmit onDrain)


handleDMA :: DMA_PERIPH -> DMA_CHANNEL -> USART_PERIPH -> Ivory eff () -> Maybe (Ivory eff ()) -> Ivory eff ()
handleDMA dmaPer dmaCh uart onTransmit onDrain = do
    f <- getInterruptFlagDMA    dmaPer dmaCh dma_int_flag_ftf
    when f $ do
        clearInterruptFlagDMA   dmaPer dmaCh dma_int_flag_ftf
        M.when (isJust onDrain) $ do
            disableInterrupt    uart usart_int_rbne
            enableInterrupt     uart usart_int_tc
        onTransmit


handleUART :: USART_PERIPH -> (Uint16 -> Ivory eff ()) -> Maybe (Ivory eff ()) -> Ivory eff ()
handleUART uart onReceive onDrain = do
    handleReceive uart onReceive
    traverse_ (handleDrain uart) onDrain

handleReceive :: USART_PERIPH -> (Uint16 -> Ivory eff ()) -> Ivory eff ()
handleReceive uart onReceive = do
    rbne  <- getInterruptFlag   uart usart_int_flag_rbne
    when rbne $ do
        clearInterruptFlag      uart usart_int_flag_rbne
        -- ferr         <- getFlag uart usart_flag_ferr
        -- nerr         <- getFlag uart usart_flag_nerr
        -- orerr        <- getFlag uart usart_flag_orerr
        -- perr         <- getFlag uart usart_flag_perr
        -- clearFlag               uart usart_flag_ferr
        -- clearFlag               uart usart_flag_nerr
        -- clearFlag               uart usart_flag_orerr
        -- clearFlag               uart usart_flag_perr
        -- when (iNot $ ferr .|| nerr .|| orerr .|| perr) $
        onReceive =<< S.receiveData uart

handleDrain :: USART_PERIPH -> Ivory eff () -> Ivory eff ()
handleDrain uart onDrain = do
    tc <- getInterruptFlag      uart usart_int_flag_tc
    when tc $ do
        clearInterruptFlag      uart usart_int_flag_tc
        disableInterrupt        uart usart_int_tc
        enableInterrupt         uart usart_int_rbne
        onDrain



instance I.UART UART where
    configUART (UART {..}) baudrate length stop parity = do
        deinitUSART         uart
        configReceive       uart usart_receive_enable
        configTransmit      uart usart_transmit_enable
        enableInterrupt     uart usart_int_rbne
        setBaudrate         uart baudrate
        setWordLength       uart $ coerceWordLength length
        setStopBit          uart $ coerceStopBit    stop
        configParity        uart $ coerceParity     parity
        enableUSART         uart



    transmit UART{..} buff n = do
        store (dmaParams ~> memory0_addr) =<< castArrayUint16ToUint32 buff
        store (dmaParams ~> number) $ safeCast n
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
