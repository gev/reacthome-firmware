{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Support.Device.GD32F4xx
    ( IRQn (..)
    , inclG
    , makeIRQHandler
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
    | USART1_IRQn
    | DMA0_Channel0
    deriving (Show, Enum, Bounded)
instance ExtDef IRQn Uint8


inclG :: ModuleM ()
inclG = inclDef (def :: Cast IRQn Uint8)


makeIRQHandler :: Show t
               => t
               -> (forall s. Ivory (ProcEffects s ()) ())
               -> ModuleM ()
makeIRQHandler t b = incl $ proc (show t <> "_IRQHandler") $ body b