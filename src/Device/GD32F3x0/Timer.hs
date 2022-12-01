module Device.GD32F3x0.Timer where

import qualified Interface                     as I
import qualified Interface.Timer               as I
import           Support.Device.GD32F3x0       (IRQn (TIMER2_IRQn), inclG)
import           Support.Device.GD32F3x0.Misc  as S
import           Support.Device.GD32F3x0.RCU   as S
import           Support.Device.GD32F3x0.Timer as S


data Timer = Timer
  { timer :: TIMER_PERIPH
  , rcu   :: RCU_PERIPH
  , irq   :: IRQn
  }

timer_2 = Timer TIMER2 RCU_TIMER2 TIMER2_IRQn

instance I.Interface Timer where
  dependencies = const [inclG, inclRCU, inclMisc, inclTimer]
  initialize t = enablePeriphClock $ rcu t


instance I.Timer Timer
