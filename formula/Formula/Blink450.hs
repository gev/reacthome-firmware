module Formula.Blink450 where

import           Core.Formula
import           Device.GD32F4xx
import           Feature.Blink   
import           Ivory.Language
import           Transport.UBUS


blink450 :: Formula GD32F4xx
blink450 = Formula { name       = "blink"
                   , model      = 0xff
                   , version    = (1, 0)
                   , shouldInit = false
                   , transport  = UBUS
                   , features   = [ blink 1 out_pd_12 ]
                   }
