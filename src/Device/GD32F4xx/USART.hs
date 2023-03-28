{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Device.GD32F4xx.USART where

import qualified Control.Monad                 as M
import           Control.Monad.Writer          (MonadWriter)
import           Core.Context
import           Core.Handler
import           Data.Maybe
import qualified Device.GD32F4xx.GPIO          as G
import           Interface.USART               (HandleUSART (onDrain))
import qualified Interface.USART               as I
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Support.Device.GD32F4xx
import           Support.Cast
import           Support.Device.GD32F4xx.DMA
import           Support.Device.GD32F4xx.IRQ
import           Support.Device.GD32F4xx.Misc
import           Support.Device.GD32F4xx.RCU
import           Support.Device.GD32F4xx.USART as S


data USART = USART
    { usart     :: USART_PERIPH
    , rcu       :: RCU_PERIPH
    , usartIRQ  :: IRQn
    , dmaRcu    :: RCU_PERIPH
    , dmaPer    :: DMA_PERIPH
    , dmaCh     :: DMA_CHANNEL
    , dmaSubPer :: DMA_SUBPERIPH
    , dmaIRQn   :: IRQn
    , dmaIRQc   :: DMA_CHANNEL_IRQ
    , rx        :: G.Port
    , tx        :: G.Port
    }



mkUSART :: MonadWriter Context m
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
        -> m USART
mkUSART usart rcu usartIRQ dmaRcu dmaPer dmaCh dmaSubPer dmaIRQn dmaIRQc rx tx = do
    addInit $ G.initPort rx
    addInit $ G.initPort tx
    addInit initUSART'
    pure USART { usart, rcu, usartIRQ, dmaRcu, dmaPer, dmaCh, dmaSubPer, dmaIRQn, dmaIRQc, rx, tx }
    where
        initUSART' :: Def ('[] ':-> ())
        initUSART' = proc (show usart <> "_init") $ body $ do
            enablePeriphClock   dmaRcu
            enableIrqNvic       usartIRQ 0 0
            enableIrqNvic       dmaIRQn  1 0
            enablePeriphClock   rcu
            deinitUSART         usart
            configReceive       usart USART_RECEIVE_ENABLE
            configTransmit      usart USART_TRANSMIT_ENABLE
            setBaudrate         usart 2_000_000
            setWordLength       usart USART_WL_8BIT
            configParity        usart USART_PM_NONE
            enableInterrupt     usart USART_INT_RBNE
            enableUSART         usart



instance Handler I.HandleUSART USART where
    addHandler (I.HandleUSART USART{..} onReceive onTransmit onDrain) = do
        addModule $ makeIRQHandler usart (handleUSART usart onReceive onDrain)
        addModule $ makeIRQHandler dmaIRQc (handleDMA dmaPer dmaCh usart onTransmit onDrain)


handleDMA :: DMA_PERIPH -> DMA_CHANNEL -> USART_PERIPH -> Ivory eff () -> Maybe (Ivory eff ()) -> Ivory eff ()
handleDMA dmaPer dmaCh usart onTransmit onDrain = do
    f <- getInterruptFlagDMA    dmaPer dmaCh DMA_INT_FLAG_FTF
    when f $ do
        clearInterruptFlagDMA   dmaPer dmaCh DMA_INT_FLAG_FTF
        M.when (isJust onDrain) $ do
            disableInterrupt    usart USART_INT_RBNE
            enableInterrupt     usart USART_INT_TC
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



    transmit USART{..} buff n = do
        deinitDMA dmaPer dmaCh
        p <- udata (def usart)
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
        transmitDMA usart USART_DENT_ENABLE
        enableInterruptDMA dmaPer dmaCh DMA_CHXCTL_FTFIE
        enableChannelDMA dmaPer dmaCh

    enable u = enableUSART (usart u)


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
