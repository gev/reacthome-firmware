module Formula.DI4 where

import           Core.Formula
import           Device.GD32F3x0
import           Feature.DInputs
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS

di4 :: Formula GD32F3x0
di4 = Formula { name       = "di4"
              , model      = 0x20
              , version    = (4, 2)
              , shouldInit = false
              , transport  = rbus $ rs485 1 uart_1 out_pa_4
              , features   = [dinputs [  in_pa_12
                                      ,  in_pa_11
                                      ,  in_pa_10
                                      ,  in_pa_9
                                      ]  
                          ]
              }