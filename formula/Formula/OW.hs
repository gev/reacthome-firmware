module Formula.OW where
import           Core.Formula

import           Device.GD32F3x0      as D
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS



ow :: Formula GD32F3x0
ow = Formula { name = "ow"
             , model      = 0xb4
             , version    = (2, 1)
             , shouldInit = true
             , transport  = rbus $ rs485 1 uart_0 out_pb_2
             , features   = [ testOW $ D.ow od_pa_8

                            ]
             }
