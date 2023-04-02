{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}

module Support.Device.GD32F3x0.IRQ
    ( IRQn

    , timer1_irqn
    , timer2_irqn
    , usart1_irqn
    , dma_channel3_4_irqn
    , exti0_1_irqn
    , exti2_3_irqn
    , exti4_15_irqn

    , makeIRQHandler

    , inclIRQ
    ) where

import           Ivory.Language
import           Ivory.Support
import           Ivory.Support.Device.GD32F3x0


newtype IRQn = IRQn Uint8
    deriving (IvoryExpr, IvoryInit, IvoryVar, IvoryType)

timer1_irqn         = IRQn $ ext "TIMER1_IRQn"
timer2_irqn         = IRQn $ ext "TIMER2_IRQn"
usart1_irqn         = IRQn $ ext "USART1_IRQn"
dma_channel3_4_irqn = IRQn $ ext "DMA_Channel3_4_IRQn"
exti0_1_irqn        = IRQn $ ext "EXTI0_1_IRQn"
exti2_3_irqn        = IRQn $ ext "EXTI2_3_IRQn"
exti4_15_irqn       = IRQn $ ext "EXTI4_15_IRQn"



makeIRQHandler :: ExtSymbol t
               => t
               -> (forall s. Ivory (ProcEffects s ()) ())
               -> ModuleDef
makeIRQHandler t b = incl $ proc (symbol t <> "_IRQHandler") $ body b



inclIRQ :: ModuleDef
inclIRQ = do
    inclSym timer1_irqn
    inclSym timer2_irqn
    inclSym usart1_irqn
    inclSym dma_channel3_4_irqn
    inclSym exti0_1_irqn
    inclSym exti2_3_irqn
    inclSym exti4_15_irqn
