{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F3x0.UART where

import qualified Control.Monad                 as M
import           Control.Monad.Writer          (MonadWriter)
import           Core.Context
import           Core.Handler
import           Data.Maybe
import qualified Device.GD32F3x0.GPIO          as G
import           Interface.UART                (HandleUART (onDrain))
import qualified Interface.UART                as I
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Support.Device.GD32F3x0
import           Support.Cast
import           Support.Device.GD32F3x0
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.IRQ
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.USART as S


data UART = UART
    { uart    :: USART_PERIPH
    , rcu     :: RCU_PERIPH
    , uartIRQ :: IRQn
    , dma     :: DMA_CHANNEL
    , dmaIRQn :: IRQn
    , rx      :: G.Port
    , tx      :: G.Port
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
mkUART uart rcu uartIRQ dma dmaIRQn dmaIRQc rx tx = do
    addInit $ G.initPort rx
    addInit $ G.initPort tx
    addInit initUART'
    pure UART { uart, rcu, uartIRQ, dma, dmaIRQn, rx, tx }
    where
        initUART' :: Def ('[] ':-> ())
        initUART' = proc (show uart <> "_init") $ body $ do
            enablePeriphClock   RCU_DMA
            enableIrqNvic       uartIRQ 0 0
            enableIrqNvic       dmaIRQn  1 0
            enablePeriphClock   rcu
            deinitUSART         uart
            configReceive       uart USART_RECEIVE_ENABLE
            configTransmit      uart USART_TRANSMIT_ENABLE
            setBaudrate         uart 1_000_000
            setWordLength       uart USART_WL_8BIT
            configParity        uart USART_PM_NONE
            enableInterrupt     uart USART_INT_RBNE
            enableUSART         uart


instance Handler I.HandleUART UART where
    addHandler (I.HandleUART UART{..} onReceive onTransmit onDrain) = do
        addModule $ makeIRQHandler uart (handleUART uart onReceive onDrain)
        addModule $ makeIRQHandler dmaIRQc (handleDMA dma uart onTransmit onDrain)


handleDMA :: DMA_CHANNEL -> USART_PERIPH -> Ivory eff () -> Maybe (Ivory eff ()) -> Ivory eff ()
handleDMA dma uart onTransmit onDrain = do
    f <- getInterruptFlagDMA    dma   DMA_INT_FLAG_FTF
    when f $ do
        clearInterruptFlagDMA   dma   DMA_INT_FLAG_G
        M.when (isJust onDrain) $ do
            disableInterrupt    uart USART_INT_RBNE
            enableInterrupt     uart USART_INT_TC
        onTransmit


handleUART :: USART_PERIPH -> (Uint16 -> Ivory eff ()) -> Maybe (Ivory eff ()) -> Ivory eff ()
handleUART uart onReceive onDrain = do
    handleReceive uart onReceive
    mapM_ (handleDrain uart) onDrain

handleReceive :: USART_PERIPH -> (Uint16 -> Ivory eff ()) -> Ivory eff ()
handleReceive uart onReceive = do
    rbne <- getInterruptFlag    uart USART_INT_FLAG_RBNE
    when rbne $ onReceive =<< S.receiveData uart

handleDrain :: USART_PERIPH -> Ivory eff () -> Ivory eff ()
handleDrain uart onDrain = do
    tc <- getInterruptFlag      uart USART_INT_FLAG_TC
    when tc $ do
        clearInterruptFlag      uart USART_INT_FLAG_TC
        disableInterrupt        uart USART_INT_TC
        enableInterrupt         uart USART_INT_RBNE
        onDrain



instance I.UART UART where
    {-
        TODO: Should we "deinit" UART before change a configuration?
    -}
    setBaudrate   u    = S.setBaudrate $ uart u
    setWordLength u wl = S.setWordLength (uart u) (coerceWordLength wl)
    setStopBit    u sb = S.setStopBit    (uart u) (coerceStopBit sb)
    setParity     u p  = S.configParity  (uart u) (coerceParity p)


    transmit UART{..} buff n = do
        deinitDMA dma
        p <- tdata (def uart)
        m <- castArrayToUint32 buff
        initDMA dma dmaInitParam { dmaPeriphAddr = p
                                 , dmaMemoryAddr = m
                                 , dmaNumber     = n
                                 }
        disableCirculationDMA dma
        disableMemoryToMemoryDMA dma
        transmitDMA uart USART_DENT_ENABLE
        enableInterruptDMA dma DMA_INT_FTF
        enableChannelDMA dma

    enable u = enableUSART (uart u)


{-
    TODO: add all values of word length, stopbit and parity
    TODO: check: are DMA structure's completed?
-}
coerceWordLength :: I.WordLength -> USART_WORD_LENGTH
coerceWordLength I.WL_8b = USART_WL_8BIT

coerceStopBit :: I.StopBit -> USART_STOP_BIT
coerceStopBit I.SB_1b = USART_STB_1BIT

coerceParity :: I.Parity -> USART_PARITY_CFG
coerceParity I.None = USART_PM_NONE


dmaInitParam :: DMA_PARAM
dmaInitParam = dmaParam { dmaDirection   = DMA_MEMORY_TO_PERIPHERAL
                        , dmaMemoryInc   = DMA_MEMORY_INCREASE_ENABLE
                        , dmaMemoryWidth = DMA_MEMORY_WIDTH_16BIT
                        , dmaPeriphInc   = DMA_PERIPH_INCREASE_DISABLE
                        , dmaPeriphWidth = DMA_PERIPHERAL_WIDTH_16BIT
                        , dmaPriority    = DMA_PRIORITY_ULTRA_HIGH
                        }
