{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.RCU
    ( RCU_PERIPH(..)
    , enablePeriphClock
    , inclRCU
    ) where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Support
import           Ivory.Support.Device.GD32F3x0

data RCU_PERIPH
    = RCU_DMA
    | RCU_GPIOA
    | RCU_GPIOB
    | RCU_TIMER1
    | RCU_TIMER2
    | RCU_USART1
    | RCU_CFGCMP
    deriving (Show, Enum, Bounded)
instance ExtDef RCU_PERIPH Uint32


inclRCU :: ModuleDef
inclRCU =    do
    inclDef (def :: Cast RCU_PERIPH Uint32)
    incl rcu_periph_clock_enable


enablePeriphClock :: RCU_PERIPH -> Ivory eff ()
enablePeriphClock = call_ rcu_periph_clock_enable . def

rcu_periph_clock_enable :: Def ('[Uint32] :-> ())
rcu_periph_clock_enable = fun "rcu_periph_clock_enable"
