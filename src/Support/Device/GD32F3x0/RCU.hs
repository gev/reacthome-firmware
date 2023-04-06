{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.RCU
    ( RCU_PERIPH

    , rcu_dma
    , rcu_gpioa
    , rcu_gpiob
    , rcu_timer1
    , rcu_timer2
    , rcu_usart1
    , rcu_cfgcmp

    , enablePeriphClock

    , inclRCU
    ) where

import           Ivory.Language
import           Ivory.Support.Device.GD32F3x0



newtype RCU_PERIPH = RCU_PERIPH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

rcu_dma    = RCU_PERIPH $ ext "RCU_DMA"
rcu_gpioa  = RCU_PERIPH $ ext "RCU_GPIOA"
rcu_gpiob  = RCU_PERIPH $ ext "RCU_GPIOB"
rcu_timer1 = RCU_PERIPH $ ext "RCU_TIMER1"
rcu_timer2 = RCU_PERIPH $ ext "RCU_TIMER2"
rcu_usart1 = RCU_PERIPH $ ext "RCU_USART1"
rcu_cfgcmp = RCU_PERIPH $ ext "RCU_CFGCMP"



enablePeriphClock :: RCU_PERIPH -> Ivory eff ()
enablePeriphClock = call_ rcu_periph_clock_enable

rcu_periph_clock_enable :: Def ('[RCU_PERIPH] :-> ())
rcu_periph_clock_enable = fun "rcu_periph_clock_enable"



inclRCU :: ModuleDef
inclRCU = do
    inclSym rcu_dma
    inclSym rcu_gpioa
    inclSym rcu_gpiob
    inclSym rcu_timer1
    inclSym rcu_timer2
    inclSym rcu_usart1
    inclSym rcu_cfgcmp

    incl rcu_periph_clock_enable
