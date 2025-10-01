{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F4xx.RCU (
    RCU_PERIPH,
    rcu_dma0,
    rcu_dma1,
    rcu_gpioa,
    rcu_gpiob,
    rcu_gpioc,
    rcu_gpiod,
    rcu_gpioe,
    rcu_timer1,
    rcu_timer2,
    rcu_timer3,
    rcu_timer6,
    rcu_timer7,
    rcu_usart0,
    rcu_usart1,
    rcu_usart2,
    rcu_usart5,
    rcu_uart3,
    rcu_uart4,
    rcu_uart6,
    rcu_uart7,
    rcu_syscfg,
    rcu_enet,
    rcu_enettx,
    rcu_enetrx,
    rcu_i2c0,
    rcu_i2c1,
    rcu_i2c2,
    rcu_spi1,
    rcu_spi2,
    rcu_timer_psc_mul4,
    enablePeriphClock,
    configRcuTimerClockPrescaler,
    inclRCU,
) where

import Ivory.Language
import Ivory.Support.Device.GD32F4xx

newtype RCU_PERIPH = RCU_PERIPH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

rcu_dma0 = RCU_PERIPH $ ext "RCU_DMA0"
rcu_dma1 = RCU_PERIPH $ ext "RCU_DMA1"
rcu_gpioa = RCU_PERIPH $ ext "RCU_GPIOA"
rcu_gpiob = RCU_PERIPH $ ext "RCU_GPIOB"
rcu_gpioc = RCU_PERIPH $ ext "RCU_GPIOC"
rcu_gpiod = RCU_PERIPH $ ext "RCU_GPIOD"
rcu_gpioe = RCU_PERIPH $ ext "RCU_GPIOE"
rcu_timer1 = RCU_PERIPH $ ext "RCU_TIMER1"
rcu_timer2 = RCU_PERIPH $ ext "RCU_TIMER2"
rcu_timer3 = RCU_PERIPH $ ext "RCU_TIMER3"
rcu_timer6 = RCU_PERIPH $ ext "RCU_TIMER6"
rcu_timer7 = RCU_PERIPH $ ext "RCU_TIMER7"
rcu_usart0 = RCU_PERIPH $ ext "RCU_USART0"
rcu_usart1 = RCU_PERIPH $ ext "RCU_USART1"
rcu_usart2 = RCU_PERIPH $ ext "RCU_USART2"
rcu_usart5 = RCU_PERIPH $ ext "RCU_USART5"
rcu_uart3 = RCU_PERIPH $ ext "RCU_UART3"
rcu_uart4 = RCU_PERIPH $ ext "RCU_UART4"
rcu_uart6 = RCU_PERIPH $ ext "RCU_UART6"
rcu_uart7 = RCU_PERIPH $ ext "RCU_UART7"
rcu_syscfg = RCU_PERIPH $ ext "RCU_SYSCFG"
rcu_enet = RCU_PERIPH $ ext "RCU_ENET"
rcu_enettx = RCU_PERIPH $ ext "RCU_ENETTX"
rcu_enetrx = RCU_PERIPH $ ext "RCU_ENETRX"
rcu_i2c0 = RCU_PERIPH $ ext "RCU_I2C0"
rcu_i2c1 = RCU_PERIPH $ ext "RCU_I2C1"
rcu_i2c2 = RCU_PERIPH $ ext "RCU_I2C2"
rcu_spi1 = RCU_PERIPH $ ext "RCU_SPI1"
rcu_spi2 = RCU_PERIPH $ ext "RCU_SPI2"

newtype TIMER_PSC_MUL = TIMER_PSC_MUL Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)
rcu_timer_psc_mul4 = TIMER_PSC_MUL $ ext "RCU_TIMER_PSC_MUL4"

enablePeriphClock :: RCU_PERIPH -> Ivory eff ()
enablePeriphClock = call_ rcu_periph_clock_enable

rcu_periph_clock_enable :: Def ('[RCU_PERIPH] :-> ())
rcu_periph_clock_enable = fun "rcu_periph_clock_enable"

configRcuTimerClockPrescaler :: TIMER_PSC_MUL -> Ivory eff ()
configRcuTimerClockPrescaler = call_ rcu_timer_clock_prescaler_config

rcu_timer_clock_prescaler_config :: Def ('[TIMER_PSC_MUL] :-> ())
rcu_timer_clock_prescaler_config = fun "rcu_timer_clock_prescaler_config"

inclRCU :: ModuleDef
inclRCU = do
    inclSym rcu_dma0
    inclSym rcu_dma1
    inclSym rcu_gpioa
    inclSym rcu_gpiob
    inclSym rcu_gpioc
    inclSym rcu_gpiod
    inclSym rcu_gpioe
    inclSym rcu_timer1
    inclSym rcu_timer2
    inclSym rcu_timer3
    inclSym rcu_timer6
    inclSym rcu_timer7
    inclSym rcu_usart0
    inclSym rcu_usart1
    inclSym rcu_usart2
    inclSym rcu_usart5
    inclSym rcu_uart3
    inclSym rcu_uart4
    inclSym rcu_uart6
    inclSym rcu_uart7
    inclSym rcu_syscfg
    inclSym rcu_enet
    inclSym rcu_enettx
    inclSym rcu_enetrx
    inclSym rcu_i2c0
    inclSym rcu_i2c1
    inclSym rcu_i2c2
    inclSym rcu_spi1
    inclSym rcu_spi2

    inclSym rcu_timer_psc_mul4

    incl rcu_periph_clock_enable
    incl rcu_timer_clock_prescaler_config
