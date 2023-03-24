{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Support.Device.GD32F4xx.IRQ
    ( IRQn (..)
    , makeIRQHandler
    , inclIRQ
    ) where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Language.Proc
import           Ivory.Language.Syntax
import           Ivory.Support
import           Ivory.Support.Device.GD32F4xx


data IRQn
    = TIMER1_IRQn
    | TIMER2_IRQn
    | USART0_IRQn
    | USART1_IRQn
    | USART5_IRQn
    | DMA0_Channel0_IRQn
    | DMA1_Channel6_IRQn
    deriving (Show, Enum, Bounded)
instance ExtDef IRQn Uint8


inclIRQ :: ModuleDef
inclIRQ = inclDef (def :: Cast IRQn Uint8)


makeIRQHandler :: Show t
               => t
               -> (forall s. Ivory (ProcEffects s ()) ())
               -> ModuleDef
makeIRQHandler t b = incl $ proc (show t <> "_IRQHandler") $ body b
