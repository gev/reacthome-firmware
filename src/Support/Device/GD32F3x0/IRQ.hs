{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}

module Support.Device.GD32F3x0.IRQ
    ( IRQn

    , timer0_irqn
    , timer1_irqn
    , timer2_irqn
    , timer14_irqn
    , timer15_irqn
    , usart0_irqn
    , usart1_irqn
    , dma_channel0_irqn
    , dma_channel1_2_irqn
    , dma_channel3_4_irqn
    , dma_channel5_6_irqn
    , exti0_1_irqn
    , exti2_3_irqn
    , exti4_15_irqn
    , i2c0_ev_irqn
    , i2c0_er_irqn
    , i2c1_ev_irqn
    , i2c1_er_irqn

    , makeIRQHandlerName
    , makeIRQHandler

    , inclIRQ
    ) where

import           Ivory.Language
import           Ivory.Support
import           Ivory.Support.Device.GD32F3x0


newtype IRQn = IRQn Uint8
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
instance ExtSymbol IRQn

timer0_irqn         = IRQn $ ext "TIMER0_IRQn"
timer1_irqn         = IRQn $ ext "TIMER1_IRQn"
timer2_irqn         = IRQn $ ext "TIMER2_IRQn"
timer14_irqn        = IRQn $ ext "TIMER14_IRQn"
timer15_irqn        = IRQn $ ext "TIMER15_IRQn"
usart0_irqn         = IRQn $ ext "USART0_IRQn"
usart1_irqn         = IRQn $ ext "USART1_IRQn"
dma_channel0_irqn   = IRQn $ ext "DMA_Channel0_IRQn"
dma_channel1_2_irqn = IRQn $ ext "DMA_Channel1_2_IRQn"
dma_channel3_4_irqn = IRQn $ ext "DMA_Channel3_4_IRQn"
dma_channel5_6_irqn = IRQn $ ext "DMA_Channel5_6_IRQn"
exti0_1_irqn        = IRQn $ ext "EXTI0_1_IRQn"
exti2_3_irqn        = IRQn $ ext "EXTI2_3_IRQn"
exti4_15_irqn       = IRQn $ ext "EXTI4_15_IRQn"
i2c0_ev_irqn        = IRQn $ ext "I2C0_EV_IRQn"
i2c0_er_irqn        = IRQn $ ext "I2C0_ER_IRQn"
i2c1_ev_irqn        = IRQn $ ext "I2C1_EV_IRQn"
i2c1_er_irqn        = IRQn $ ext "I2C1_ER_IRQn"



makeIRQHandlerName :: IRQn -> String
makeIRQHandlerName t = (init . symbol) t <> "Handler"


makeIRQHandler :: IRQn
               -> (forall s. Ivory (ProcEffects s ()) ())
               -> ModuleDef
makeIRQHandler t b = incl $ proc (makeIRQHandlerName t) $ body b



inclIRQ :: ModuleDef
inclIRQ = do
    inclSym timer0_irqn
    inclSym timer1_irqn
    inclSym timer2_irqn
    inclSym timer14_irqn
    inclSym timer15_irqn
    inclSym usart0_irqn
    inclSym usart1_irqn
    inclSym dma_channel0_irqn
    inclSym dma_channel1_2_irqn
    inclSym dma_channel3_4_irqn
    inclSym dma_channel5_6_irqn
    inclSym exti0_1_irqn
    inclSym exti2_3_irqn
    inclSym exti4_15_irqn
    inclSym i2c0_ev_irqn
    inclSym i2c0_er_irqn
    inclSym i2c1_ev_irqn
    inclSym i2c1_er_irqn
