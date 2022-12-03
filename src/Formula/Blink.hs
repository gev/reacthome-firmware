module Formula.Blink where

import           Device.GD32F3x0.GPIO
import           Device.GD32F3x0.Timer
import           Feature
import           Feature.Blink
import           Formula
import           Support.Device.GD32F3x0.Timer


blink :: Formula
blink = Formula { systemClock = timer_2_irq
                                timerParam { prescaler = 8399
                                           , period = 9
                                           }
                , features = [ Feature $ Blink 1 out_pa_15 ]
                }
