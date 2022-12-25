{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}

module Device.GD32F3x0.USART where

import qualified Device.GD32F3x0.GPIO          as G
import           Include
import           Initialize
import qualified Interface.USART               as I
import           Ivory.Language
import           Ivory.Stdlib
import           Ivory.Support.Device.GD32F3x0
import           Support.Device.GD32F3x0
import           Support.Device.GD32F3x0.DMA
import           Support.Device.GD32F3x0.GPIO  (GPIO_AF (GPIO_AF_1))
import           Support.Device.GD32F3x0.Misc
import           Support.Device.GD32F3x0.RCU
import           Support.Device.GD32F3x0.USART as S
import           Support.Util


data USART = USART
  { usart    :: USART_PERIPH
  , rcu      :: RCU_PERIPH
  , usartIRQ :: IRQn
  , dma      :: DMA_CHANNEL
  , dmaIRQn  :: IRQn
  , dmaIRQc  :: DMA_CHANNEL_IRQ
  , rx       :: G.PORT
  , tx       :: G.PORT
  }

usart_1 = USART USART1
                RCU_USART1
                USART1_IRQn
                DMA_CH3
                DMA_Channel3_4_IRQn
                DMA_Channel3_4
                (G.pa_3 $ G.AF GPIO_AF_1)
                (G.pa_2 $ G.AF GPIO_AF_1)



instance Include (I.HandleUSART USART) where
  include (I.HandleUSART (USART {usart, dma, dmaIRQc}) onReceive onTransmit onDrain) =
    inclG >> inclMisc >> inclUSART >> inclDMA >> inclUtil >> G.include' >>
    makeIRQHandler dmaIRQc (handleDMA dma usart onTransmit) >>
    makeIRQHandler usart (handleUSART usart onReceive onDrain)


handleDMA :: DMA_CHANNEL -> USART_PERIPH -> Ivory eff () -> Ivory eff ()
handleDMA dma usart onTransmit = do
  f <- getInterruptFlagDMA dma DMA_INT_FLAG_FTF
  when f $ do
    clearInterruptFlagDMA dma DMA_INT_FLAG_G
    disableInterrupt usart USART_INT_RBNE
    enableInterrupt  usart USART_INT_TC
    onTransmit


handleUSART :: USART_PERIPH -> (Uint16 -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
handleUSART usart onReceive onDrain = do
  rbne <- getInterruptFlag usart USART_INT_FLAG_RBNE
  when rbne $ onReceive =<< S.receiveData usart
  tc <- getInterruptFlag usart USART_INT_FLAG_TC
  when tc $ do
    clearInterruptFlag usart USART_INT_FLAG_TC
    disableInterrupt   usart USART_INT_TC
    enableInterrupt    usart USART_INT_RBNE
    onDrain



instance Initialize USART where
  initialize (USART {usart, rcu, usartIRQ, dmaIRQn, rx, tx}) =
    G.initialize' rx : G.initialize' tx : [
      proc (show usart <> "_init") $ body $ do
        enablePeriphClock RCU_DMA
        enableIrqNvic     usartIRQ 0 0
        enableIrqNvic     dmaIRQn  1 0
        enablePeriphClock rcu
        deinitUSART     usart
        configReceive   usart USART_RECEIVE_ENABLE
        configTransmit  usart USART_TRANSMIT_ENABLE
        setBaudrate     usart 1_000_000
        setWordLength   usart USART_WL_8BIT
        configParity    usart USART_PM_NONE
        enableInterrupt usart USART_INT_RBNE
        enableUSART     usart
    ]


instance I.USART USART where
  {-
    TODO: Should we "deinit" USART before change a configuration?
  -}
  setBaudrate   u     = S.setBaudrate $ usart u
  setWordLength u wl  = S.setWordLength (usart u) (coerceWordLength wl)
  setStopBit    u sb  = S.setStopBit    (usart u) (coerceStopBit sb)
  setParity     u p   = S.configParity  (usart u) (coerceParity p)


  transmit (USART {usart, dma}) buff n = do
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

  enable         u  = enableUSART (usart u)


{-
  TODO: add all values of word length, stopbit and parity
-}
coerceWordLength :: I.WordLength -> USART_WORD_LENGTH
coerceWordLength I.WL_8b = USART_WL_8BIT

coerceStopBit :: I.StopBit -> USART_STOP_BIT
coerceStopBit I.SB_1b = USART_STB_1BIT

coerceParity :: I.Parity -> USART_PARITY_CFG
coerceParity I.None = USART_PM_NONE


dmaInitParam :: DMA_PARAM
dmaInitParam = dmaParam { dmaDirection    = DMA_MEMORY_TO_PERIPHERAL
                        , dmaMemoryInc    = DMA_MEMORY_INCREASE_ENABLE
                        , dmaMemoryWidth  = DMA_MEMORY_WIDTH_16BIT
                        , dmaPeriphInc    = DMA_PERIPH_INCREASE_DISABLE
                        , dmaPeriphWidth  = DMA_PERIPHERAL_WIDTH_16BIT
                        , dmaPriority     = DMA_PRIORITY_ULTRA_HIGH
                        }
