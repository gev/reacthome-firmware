{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RankNTypes        #-}

module Device.GD32F3x0.USART where

import           Device.GD32F3x0.GPIO
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
  { usart :: USART_PERIPH
  , rcu   :: RCU_PERIPH
  , irq   :: IRQn
  , dma   :: DMA_CHANNEL
  , rx    :: PORT
  , tx    :: PORT
  }

usart_1 = USART USART1
                RCU_USART1
                USART1_IRQn
                DMA_CH3
                (pa_3 $ AF GPIO_AF_1)
                (pa_2 $ AF GPIO_AF_1)

instance Include (I.HandleUSART USART) where
  include (I.HandleUSART (USART {usart}) onReceive onDrain) =
    inclG >> inclMisc >> inclUSART >> inclDMA >> inclUtil >> include' >>
    makeIRQHandler usart (handleIRQ usart onReceive onDrain)


instance Initialize USART where
  initialize (USART usart rcu irq dma rx tx) =
    initialize' rx : initialize' tx : [
      proc (show usart <> "_init") $ body $ do
        enablePeriphClock RCU_DMA
        enableIrqNvic     irq 0 0
        enablePeriphClock rcu
        deinitUSART     usart
        configReceive   usart USART_RECEIVE_ENABLE
        configTransmit  usart USART_TRANSMIT_ENABLE
        enableInterrupt usart USART_INT_RBNE
        enableInterrupt usart USART_INT_TC
        -- setBaudrate     usart 1_000_000
        -- setWordLength   usart USART_WL_8BIT
        -- configParity    usart USART_PM_NONE
        -- enableUSART     usart
    ]

handleIRQ :: USART_PERIPH -> (Uint16 -> Ivory eff ()) -> Ivory eff () -> Ivory eff ()
handleIRQ usart onReceive onDrain = do
  rbne <- getInterruptFlag usart USART_INT_FLAG_RBNE
  when rbne $ onReceive =<< S.receiveData usart
  tc <- getInterruptFlag usart USART_INT_FLAG_TC
  when tc $ do
    clearInterruptFlag usart USART_INT_FLAG_TC
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
    p <- ptrOf =<< tdata (def usart)
    m <- castArrayToUint32 buff
    initDMA dma dmaInitParam { dmaPeriphAddr = p
                             , dmaMemoryAddr = m
                             , dmaNumber     = n
                             }
    disableCirculationDMA dma
    disableMemoryToMemoryDMA dma
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

dmaInitParam = dmaParam { dmaDirection    = DMA_MEMORY_TO_PERIPHERAL
                        , dmaMemoryInc    = DMA_MEMORY_INCREASE_ENABLE
                        , dmaMemoryWidth  = DMA_MEMORY_WIDTH_16BIT
                        , dmaPeriphInc    = DMA_PERIPH_INCREASE_DISABLE
                        , dmaPeriphWidth  = DMA_PERIPHERAL_WIDTH_16BIT
                        , dmaPriority     = DMA_PRIORITY_ULTRA_HIGH
                        }
