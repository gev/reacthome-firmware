module Device.GD32F3x0.IRQ where

import qualified Support.Device.GD32F3x0 as S

data IRQ s = IRQ
  { irq    :: S.IRQn
  , source :: s
  }
