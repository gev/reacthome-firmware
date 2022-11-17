{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Support.Device.GD32F3x0.RCU
  ( RCU_PERIPH(..)
  , enablePeriphClock
  , inclRCU
  ) where

import           Data.Foldable
import           Ivory.Language
import           Ivory.Language.Module
import           Ivory.Language.Proc
import           Ivory.Language.Syntax

data RCU_PERIPH
  -- AHB peripherals
  = RCU_DMA
  | RCU_CRC
  | RCU_GPIOA
  | RCU_GPIOB
  | RCU_GPIOC
  | RCU_GPIOD
  | RCU_GPIOF
  | RCU_TSI
  --  APB2 peripherals
  | RCU_CFGCMP
  | RCU_ADC
  | RCU_TIMER0
  | RCU_SPI0
  | RCU_USART0
  | RCU_TIMER14
  | RCU_TIMER15
  | RCU_TIMER16
  --  APB1 peripherals
  | RCU_TIMER1
  | RCU_TIMER2
  | RCU_TIMER13
  | RCU_WWDGT
  | RCU_SPI1
  | RCU_USART1
  | RCU_I2C0
  | RCU_I2C1
  | RCU_PMU
  | RCU_RTC
  -- RCU_ADDAPB1EN
  | RCU_CTC
  deriving (Show, Enum)

enablePeriphClock :: RCU_PERIPH -> Ivory eff ()
enablePeriphClock = call_ rcuPeriphClockEnable . extConst

inclRCU :: ModuleM ()
inclRCU = do
  traverse_ (inclSym . extPeriph) [RCU_DMA .. RCU_CTC]
  incl rcuPeriphClockEnable

rcuPeriphClockEnable :: Def ('[Uint32] :-> ())
rcuPeriphClockEnable = extProc "rcu_periph_clock_enable"

extPeriph :: RCU_PERIPH -> Uint32
extPeriph = extConst

extConst :: (Show a, IvoryExpr e) => a -> e
extConst = (`extern` headerFile) . show

extProc :: ProcType t => Sym -> Def t
extProc = (`importProc` headerFile)

headerFile :: String
headerFile = "gd32f3x0_rcu.h"

