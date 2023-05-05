{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F3x0.UART where

import qualified Control.Monad                 as M
import           Control.Monad.Writer          (MonadWriter)
import           Core.Context
import           Core.Handler
import           Data.Maybe
import           Data.Record
import qualified Device.GD32F3x0.GPIO          as G
import           Interface.UART                (HandleUART (onDrain))
import qualified Interface.UART                as I
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Support
import           Ivory.Support.Device.GD32F3x0
import           Support.Cast
import           Support.Device.GD32F3x0
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.USART as S


data UART = UART
    { uart      :: USART_PERIPH
    , rcu       :: RCU_PERIPH
    , uartIRQ   :: IRQn
    , dma       :: DMA_CHANNEL
    , dmaIRQn   :: IRQn
    , dmaParams :: Record DMA_PARAM_STRUCT
    , rx        :: G.Port
    , tx        :: G.Port
    }


mkUART :: MonadWriter Context m
       => USART_PERIPH
       -> RCU_PERIPH
       -> IRQn
       -> DMA_CHANNEL
       -> IRQn
       -> G.Port
       -> G.Port
       -> m UART
mkUART uart rcu uartIRQ dma dmaIRQn rx tx = do

    let dmaInit = dmaParam [ direction    .= ival dma_memory_to_peripheral
                           , memory_inc   .= ival dma_memory_increase_enable
                           , memory_width .= ival dma_memory_width_16bit
                           , periph_inc   .= ival dma_periph_increase_disable
                           , periph_width .= ival dma_peripheral_width_16bit
                           , priority     .= ival dma_priority_ultra_high
                           ]
    dmaParams <- record (symbol uart <> "_dma_param") dmaInit

    let initUART' :: Def ('[] ':-> ())
        initUART' = proc (symbol uart <> "_init") $ body $ do
            store (dmaParams ~> periph_addr) =<< tdata uart
            enablePeriphClock   rcu_dma
            enableIrqNvic       uartIRQ 0 0
            enableIrqNvic       dmaIRQn 1 0
            enablePeriphClock   rcu

    addInit $ G.initPort rx
    addInit $ G.initPort tx
    addInit initUART'

    pure UART { uart, rcu, uartIRQ, dma, dmaIRQn, dmaParams, rx, tx }

instance Handler I.HandleUART UART where
    addHandler (I.HandleUART UART{..} onReceive onTransmit onDrain) = do
        addModule $ makeIRQHandler uartIRQ (handleUART uart onReceive onDrain)
        addModule $ makeIRQHandler dmaIRQn (handleDMA dma uart onTransmit onDrain)


handleDMA :: DMA_CHANNEL -> USART_PERIPH -> Ivory eff () -> Maybe (Ivory eff ()) -> Ivory eff ()
handleDMA dma uart onTransmit onDrain = do
    f <- getInterruptFlagDMA    dma   dma_int_flag_ftf
    when f $ do
        clearInterruptFlagDMA   dma   dma_int_flag_g
        M.when (isJust onDrain) $ do
            disableInterrupt    uart usart_int_rbne
            enableInterrupt     uart usart_int_tc
        onTransmit


handleUART :: USART_PERIPH -> (Uint16 -> Ivory eff ()) -> Maybe (Ivory eff ()) -> Ivory eff ()
handleUART uart onReceive onDrain = do
    handleReceive uart onReceive
    mapM_ (handleDrain uart) onDrain

handleReceive :: USART_PERIPH -> (Uint16 -> Ivory eff ()) -> Ivory eff ()
handleReceive uart onReceive = do
    rbne <- getInterruptFlag    uart usart_int_flag_rbne
    when rbne $ onReceive =<< S.receiveData uart

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
        store (dmaParams ~> memory_addr) =<< castArrayToUint32 buff
        store (dmaParams ~> number) $ safeCast n
        deinitDMA dma
        initDMA dma dmaParams
        disableCirculationDMA dma
        disableMemoryToMemoryDMA dma
        transmitDMA uart usart_dent_enable
        enableInterruptDMA dma dma_int_ftf
        enableChannelDMA dma

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
