{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeOperators      #-}

module Device.GD32F3x0.USART where

import           Device.GD32F3x0.GPIO
import           Feature.USART                 (onTransmit)
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
  , dmaIRQ   :: IRQn
  , rx       :: PORT
  , tx       :: PORT
  }

usart_1 = USART USART1
                RCU_USART1
                USART1_IRQn
                DMA_CH3
                DMA_Channel3_4_IRQn
                (pa_3 $ AF GPIO_AF_1)
                (pa_2 $ AF GPIO_AF_1)


instance Include (I.HandleUSART USART) where
  include (I.HandleUSART (USART {usart, dma}) onReceive onTransmit onDrain) =
    inclG >> inclMisc >> inclUSART >> inclDMA >> inclUtil >> include' >>
    makeIRQHandler usart (handleIRQ usart onReceive onDrain) >>
    {-
      TODO: Add DMA IRQ handler
    -}
    incl (dmaIRQHandler dma usart)


dmaIRQHandler :: DMA_CHANNEL -> USART_PERIPH -> Def ('[] ':-> ())
dmaIRQHandler dma usart = proc "DMA_Channel3_4_IRQHandle" $ body $ do
  disableInterrupt usart USART_INT_RBNE
  enableInterrupt  usart USART_INT_TC
  onTransmit

instance Initialize USART where
  initialize (USART usart rcu usartIRQ dma dmaIRQ rx tx) =
    initialize' rx : initialize' tx : [
      proc (show usart <> "_init") $ body $ do
        enablePeriphClock RCU_DMA
        enableIrqNvic     usartIRQ 0 0
        enableIrqNvic     dmaIRQ   0 0
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


handleIRQ :: USART_PERIPH -> (Uint16 -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
handleIRQ usart onReceive onDrain = do
  rbne <- getInterruptFlag usart USART_INT_FLAG_RBNE
  when rbne $ onReceive =<< S.receiveData usart
  tc <- getInterruptFlag usart USART_INT_FLAG_TC
  when tc $ do
    clearInterruptFlag usart USART_INT_FLAG_TC
    disableInterrupt usart USART_INT_TC
    enableInterrupt  usart USART_INT_RBNE
    onDrain


instance I.USART USART where

  {-
    TODO: Should we "deinit" USART before change a configuration?
  -}
  setBaudrate   u     = S.setBaudrate $ usart u
  setWordLength u wl  = S.setWordLength (usart u) (coerceWordLength wl)
  setStopBit    u sb  = S.setStopBit    (usart u) (coerceStopBit sb)
  setParity     u p   = S.configParity  (usart u) (coerceParity p)

  receive = S.receiveData . usart

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
    enableInterruptDMA dma DMA_INT_FTF
    enableChannelDMA dma
    transmitDMA usart USART_DENT_ENABLE


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
