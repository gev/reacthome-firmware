{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F3x0.USART where

import           Control.Monad.Writer          (MonadWriter)
import           Core.Context
import           Core.Handler
import qualified Device.GD32F3x0.GPIO          as G
import qualified Interface.USART               as I
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


data USART = USART
    { usart    :: USART_PERIPH
    , rcu      :: RCU_PERIPH
    , usartIRQ :: IRQn
    , dma      :: DMA_CHANNEL
    , dmaIRQn  :: IRQn
    , dmaIRQc  :: DMA_CHANNEL_IRQ
    , rx       :: G.Port
    , tx       :: G.Port
    }


mkUSART :: MonadWriter Context m
        => USART_PERIPH
        -> RCU_PERIPH
        -> IRQn
        -> DMA_CHANNEL
        -> IRQn
        -> DMA_CHANNEL_IRQ
        -> G.Port
        -> G.Port
        -> m USART
mkUSART usart rcu usartIRQ dma dmaIRQn dmaIRQc rx tx = do
    addInit $ G.initPort rx
    addInit $ G.initPort tx
    addInit initUSART'
    pure USART { usart, rcu, usartIRQ, dma, dmaIRQn, dmaIRQc, rx, tx }
    where
        initUSART' :: Def ('[] ':-> ())
        initUSART' = proc (show usart <> "_init") $ body $ do
            enablePeriphClock   RCU_DMA
            enableIrqNvic       usartIRQ 0 0
            enableIrqNvic       dmaIRQn  1 0
            enablePeriphClock   rcu
            deinitUSART         usart
            configReceive       usart USART_RECEIVE_ENABLE
            configTransmit      usart USART_TRANSMIT_ENABLE
            setBaudrate         usart 1_000_000
            setWordLength       usart USART_WL_8BIT
            configParity        usart USART_PM_NONE
            enableInterrupt     usart USART_INT_RBNE
            enableUSART         usart


instance Handler I.HandleUSART USART where
    addHandler (I.HandleUSART (USART {..}) onReceive onTransmit onDrain) = do
        addModule $ makeIRQHandler usart (handleUSART usart onReceive onDrain)
        addModule $ makeIRQHandler dmaIRQc (handleDMA dma usart onTransmit)


handleDMA :: DMA_CHANNEL -> USART_PERIPH -> Ivory eff () -> Ivory eff ()
handleDMA dma usart onTransmit = do
    f <- getInterruptFlagDMA    dma   DMA_INT_FLAG_FTF
    when f $ do
        clearInterruptFlagDMA   dma   DMA_INT_FLAG_G
        disableInterrupt        usart USART_INT_RBNE
        enableInterrupt         usart USART_INT_TC
        onTransmit


handleUSART :: USART_PERIPH -> (Uint16 -> Ivory eff ()) -> Maybe (Ivory eff ()) -> Ivory eff ()
handleUSART usart onReceive onDrain = do
    handleReceive usart onReceive
    mapM_ (handleDrain usart) onDrain

handleReceive :: USART_PERIPH -> (Uint16 -> Ivory eff ()) -> Ivory eff ()
handleReceive usart onReceive = do
    rbne <- getInterruptFlag    usart USART_INT_FLAG_RBNE
    when rbne $ onReceive =<< S.receiveData usart

handleDrain :: USART_PERIPH -> Ivory eff () -> Ivory eff ()
handleDrain usart onDrain = do
    tc <- getInterruptFlag      usart USART_INT_FLAG_TC
    when tc $ do
        clearInterruptFlag      usart USART_INT_FLAG_TC
        disableInterrupt        usart USART_INT_TC
        enableInterrupt         usart USART_INT_RBNE
        onDrain



instance I.USART USART where
    {-
        TODO: Should we "deinit" USART before change a configuration?
    -}
    setBaudrate   u    = S.setBaudrate $ usart u
    setWordLength u wl = S.setWordLength (usart u) (coerceWordLength wl)
    setStopBit    u sb = S.setStopBit    (usart u) (coerceStopBit sb)
    setParity     u p  = S.configParity  (usart u) (coerceParity p)


    transmit (USART {..}) buff n = do
        deinitDMA dma
        p <- tdata (def usart)
        m <- castArrayToUint32 buff
        initDMA dma dmaInitParam { dmaPeriphAddr = p
                                 , dmaMemoryAddr = m
                                 , dmaNumber     = n
                                 }
        disableCirculationDMA dma
        disableMemoryToMemoryDMA dma
        transmitDMA usart USART_DENT_ENABLE
        enableInterruptDMA dma DMA_INT_FTF
        enableChannelDMA dma

    enable u = enableUSART (usart u)


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
