{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F4xx.RCU
    ( RCU_PERIPH(..)
    , enablePeriphClock
    , inclRCU
    ) where

import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Support
import           Ivory.Support.Device.GD32F4xx

data RCU_PERIPH
    = RCU_DMA0
    | RCU_DMA1
    | RCU_GPIOA
    | RCU_GPIOB
    | RCU_GPIOC
    | RCU_GPIOD
    | RCU_GPIOE
    | RCU_TIMER1
    | RCU_TIMER2
    | RCU_USART0
    | RCU_USART1
    | RCU_USART2
    | RCU_USART5
    | RCU_UART3
    | RCU_UART4
    | RCU_UART6
    | RCU_UART7
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
