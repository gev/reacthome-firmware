{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}

module Support.Device.GD32F4xx.IRQ
    ( IRQn
    , timer1_irqn
    , timer2_irqn
    , usart0_irqn
    , usart1_irqn
    , usart2_irqn
    , usart5_irqn
    , uart3_irqn
    , uart4_irqn
    , uart6_irqn
    , uart7_irqn
    , dma0_channel0_irqn
    , dma0_channel1_irqn
    , dma0_channel2_irqn
    , dma0_channel3_irqn
    , dma0_channel4_irqn
    , dma0_channel5_irqn
    , dma0_channel6_irqn
    , dma0_channel7_irqn
    , dma1_channel0_irqn
    , dma1_channel1_irqn
    , dma1_channel2_irqn
    , dma1_channel3_irqn
    , dma1_channel4_irqn
    , dma1_channel5_irqn
    , dma1_channel6_irqn
    , dma1_channel7_irqn

    , makeIRQHandler

    , inclIRQ
    ) where

import           Ivory.Language
import           Ivory.Support
import           Ivory.Support.Device.GD32F4xx


newtype IRQn = IRQn Uint8
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
instance ExtSymbol IRQn

timer1_irqn        = IRQn $ ext "TIMER1_IRQn"
timer2_irqn        = IRQn $ ext "TIMER2_IRQn"
usart0_irqn        = IRQn $ ext "USART0_IRQn"
usart1_irqn        = IRQn $ ext "USART1_IRQn"
usart2_irqn        = IRQn $ ext "USART2_IRQn"
usart5_irqn        = IRQn $ ext "USART5_IRQn"
uart3_irqn         = IRQn $ ext "UART3_IRQn"
uart4_irqn         = IRQn $ ext "UART4_IRQn"
uart6_irqn         = IRQn $ ext "UART6_IRQn"
uart7_irqn         = IRQn $ ext "UART7_IRQn"
dma0_channel0_irqn = IRQn $ ext "DMA0_Channel0_IRQn"
dma0_channel1_irqn = IRQn $ ext "DMA0_Channel1_IRQn"
dma0_channel2_irqn = IRQn $ ext "DMA0_Channel2_IRQn"
dma0_channel3_irqn = IRQn $ ext "DMA0_Channel3_IRQn"
dma0_channel4_irqn = IRQn $ ext "DMA0_Channel4_IRQn"
dma0_channel5_irqn = IRQn $ ext "DMA0_Channel5_IRQn"
dma0_channel6_irqn = IRQn $ ext "DMA0_Channel6_IRQn"
dma0_channel7_irqn = IRQn $ ext "DMA0_Channel7_IRQn"
dma1_channel0_irqn = IRQn $ ext "DMA1_Channel0_IRQn"
dma1_channel1_irqn = IRQn $ ext "DMA1_Channel1_IRQn"
dma1_channel2_irqn = IRQn $ ext "DMA1_Channel2_IRQn"
dma1_channel3_irqn = IRQn $ ext "DMA1_Channel3_IRQn"
dma1_channel4_irqn = IRQn $ ext "DMA1_Channel4_IRQn"
dma1_channel5_irqn = IRQn $ ext "DMA1_Channel5_IRQn"
dma1_channel6_irqn = IRQn $ ext "DMA1_Channel6_IRQn"
dma1_channel7_irqn = IRQn $ ext "DMA1_Channel7_IRQn"



inclIRQ :: ModuleDef
inclIRQ = do
    inclSym timer1_irqn
    inclSym timer2_irqn
    inclSym usart0_irqn
    inclSym usart1_irqn
    inclSym usart2_irqn
    inclSym usart5_irqn
    inclSym uart3_irqn
    inclSym uart4_irqn
    inclSym uart6_irqn
    inclSym uart7_irqn
    inclSym dma0_channel0_irqn
    inclSym dma0_channel1_irqn
    inclSym dma0_channel2_irqn
    inclSym dma0_channel3_irqn
    inclSym dma0_channel4_irqn
    inclSym dma0_channel5_irqn
    inclSym dma0_channel6_irqn
    inclSym dma0_channel7_irqn
    inclSym dma1_channel0_irqn
    inclSym dma1_channel1_irqn
    inclSym dma1_channel2_irqn
    inclSym dma1_channel3_irqn
    inclSym dma1_channel4_irqn
    inclSym dma1_channel5_irqn
    inclSym dma1_channel6_irqn
    inclSym dma1_channel7_irqn


makeIRQHandler :: IRQn
               -> (forall s. Ivory (ProcEffects s ()) ())
               -> ModuleDef
makeIRQHandler t b = incl $ proc ((init . symbol) t <> "_Handler") $ body b
