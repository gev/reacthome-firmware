{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F4xx.UART where

import qualified Control.Monad                 as M
import           Control.Monad.Writer          (MonadWriter)
import           Core.Context
import           Core.Handler
import           Data.Maybe
import qualified Device.GD32F4xx.GPIO          as G
import           Interface.UART                (HandleUART (onDrain))
import qualified Interface.UART                as I
import           Ivory.Language
import           Ivory.Stdlib
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
    , dmaIRQc   :: DMA_CHANNEL_IRQ
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
       -> DMA_CHANNEL_IRQ
       -> G.Port
       -> G.Port
       -> m UART
mkUART uart rcu uartIRQ dmaRcu dmaPer dmaCh dmaSubPer dmaIRQn dmaIRQc rx tx = do
    addInit $ G.initPort rx
    addInit $ G.initPort tx
    addInit initUART'
    pure UART { uart, rcu, uartIRQ, dmaRcu, dmaPer, dmaCh, dmaSubPer, dmaIRQn, dmaIRQc, rx, tx }
    where
        initUART' :: Def ('[] ':-> ())
        initUART' = proc (show uart <> "_init") $ body $ do
            enablePeriphClock   dmaRcu
            enableIrqNvic       uartIRQ 0 0
            enableIrqNvic       dmaIRQn  1 0
            enablePeriphClock   rcu
            deinitUSART         uart
            configReceive       uart USART_RECEIVE_ENABLE
            configTransmit      uart USART_TRANSMIT_ENABLE
            setBaudrate         uart 2_000_000
            setWordLength       uart USART_WL_8BIT
            configParity        uart USART_PM_NONE
            enableInterrupt     uart USART_INT_RBNE
            enableUSART         uart



instance Handler I.HandleUART UART where
    addHandler (I.HandleUART UART{..} onReceive onTransmit onDrain) = do
        addModule $ makeIRQHandler uart (handleUART uart onReceive onDrain)
        addModule $ makeIRQHandler dmaIRQc (handleDMA dmaPer dmaCh uart onTransmit onDrain)


handleDMA :: DMA_PERIPH -> DMA_CHANNEL -> USART_PERIPH -> Ivory eff () -> Maybe (Ivory eff ()) -> Ivory eff ()
handleDMA dmaPer dmaCh uart onTransmit onDrain = do
    f <- getInterruptFlagDMA    dmaPer dmaCh DMA_INT_FLAG_FTF
    when f $ do
        clearInterruptFlagDMA   dmaPer dmaCh DMA_INT_FLAG_FTF
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
        deinitDMA dmaPer dmaCh
        p <- udata (def uart)
        m <- castArrayToUint32 buff
        initSingleDMA dmaPer dmaCh dmaInitParam { dmaPeriphAddr = p
                                                , dmaMemoryAddr = m
                                                , dmaNumber     = safeCast n
                                                {-
                                                    TODO: Check type of dmaNumber for GD32F3x0
                                                -}
                                                }
        disableCirculationDMA dmaPer dmaCh
        selectChannelSubperipheralDMA dmaPer dmaCh dmaSubPer
        transmitDMA uart USART_DENT_ENABLE
        enableInterruptDMA dmaPer dmaCh DMA_CHXCTL_FTFIE
        enableChannelDMA dmaPer dmaCh

    enable u = enableUSART (uart u)


{-
    TODO: add all values of word length, stopbit and parity
-}
coerceWordLength :: I.WordLength -> USART_WORD_LENGTH
coerceWordLength I.WL_8b = USART_WL_8BIT

coerceStopBit :: I.StopBit -> USART_STOP_BIT
coerceStopBit I.SB_1b = USART_STB_1BIT

coerceParity :: I.Parity -> USART_PARITY_CFG
coerceParity I.None = USART_PM_NONE


dmaInitParam :: DMA_SINGLE_PARAM
dmaInitParam = dmaParam { dmaPeriphInc         =  DMA_PERIPH_INCREASE_DISABLE
                        , dmaMemoryInc         =  DMA_MEMORY_INCREASE_ENABLE
                        , dmaPeriphMemoryWidth =  DMA_PERIPH_WIDTH_16BIT
                        , dmaCircularMode      =  DMA_CIRCULAR_MODE_DISABLE
                        , dmaDirection         =  DMA_MEMORY_TO_PERIPH
                        , dmaPriority          =  DMA_PRIORITY_ULTRA_HIGH
                        }
