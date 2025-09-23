{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Support.Device.GD32F3x0.RCU (
    RCU_PERIPH,
    rcu_dma,
    rcu_gpioa,
    rcu_gpiob,
    rcu_timer0,
    rcu_timer1,
    rcu_timer2,
    rcu_timer14,
    rcu_timer15,
    rcu_usart0,
    rcu_usart1,
    rcu_cfgcmp,
    rcu_i2c0,
    rcu_i2c1,
    rcu_adc,
    rcu_dac,
    rcu_adcck_apb2_div2,
    enablePeriphClock,
    configClockADC,
    inclRCU,
) where

import Ivory.Language
import Ivory.Support.Device.GD32F3x0

newtype RCU_PERIPH = RCU_PERIPH Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

rcu_dma = RCU_PERIPH $ ext "RCU_DMA"
rcu_gpioa = RCU_PERIPH $ ext "RCU_GPIOA"
rcu_gpiob = RCU_PERIPH $ ext "RCU_GPIOB"
rcu_timer0 = RCU_PERIPH $ ext "RCU_TIMER0"
rcu_timer1 = RCU_PERIPH $ ext "RCU_TIMER1"
rcu_timer2 = RCU_PERIPH $ ext "RCU_TIMER2"
rcu_timer14 = RCU_PERIPH $ ext "RCU_TIMER14"
rcu_timer15 = RCU_PERIPH $ ext "RCU_TIMER15"
rcu_usart0 = RCU_PERIPH $ ext "RCU_USART0"
rcu_usart1 = RCU_PERIPH $ ext "RCU_USART1"
rcu_cfgcmp = RCU_PERIPH $ ext "RCU_CFGCMP"
rcu_i2c0 = RCU_PERIPH $ ext "RCU_I2C0"
rcu_i2c1 = RCU_PERIPH $ ext "RCU_I2C1"
rcu_adc = RCU_PERIPH $ ext "RCU_ADC"
rcu_dac = RCU_PERIPH $ ext "RCU_DAC"

newtype RCU_CLOCK_ADC = RCU_CLOCK_ADC Uint32
    deriving (IvoryExpr, IvoryInit, IvoryStore, IvoryType, IvoryVar)

rcu_adcck_apb2_div2 = RCU_CLOCK_ADC $ ext "RCU_ADCCK_APB2_DIV2"

enablePeriphClock :: RCU_PERIPH -> Ivory eff ()
enablePeriphClock = call_ rcu_periph_clock_enable

rcu_periph_clock_enable :: Def ('[RCU_PERIPH] :-> ())
rcu_periph_clock_enable = fun "rcu_periph_clock_enable"

configClockADC :: RCU_CLOCK_ADC -> Ivory eff ()
configClockADC = call_ rcu_adc_clock_config

rcu_adc_clock_config :: Def ('[RCU_CLOCK_ADC] :-> ())
rcu_adc_clock_config = fun "rcu_adc_clock_config"

inclRCU :: ModuleDef
inclRCU = do
    inclSym rcu_dma
    inclSym rcu_gpioa
    inclSym rcu_gpiob
    inclSym rcu_timer0
    inclSym rcu_timer1
    inclSym rcu_timer2
    inclSym rcu_timer14
    inclSym rcu_timer15
    inclSym rcu_usart0
    inclSym rcu_usart1
    inclSym rcu_cfgcmp
    inclSym rcu_i2c0
    inclSym rcu_i2c1
    inclSym rcu_adc
    inclSym rcu_dac

    inclSym rcu_adcck_apb2_div2

    incl rcu_periph_clock_enable
    incl rcu_adc_clock_config
