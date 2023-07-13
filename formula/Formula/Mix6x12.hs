module Formula.Mix6x12 where

import           Core.Formula
import           Device.GD32F3x0
import           Feature.Indicator
import           Feature.Mix          (mix)
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS


mix6x12 :: Formula GD32F3x0
mix6x12 = Formula { name       = "mix6x12"
                  , model      = 0xb5
                  , version    = (3, 1)
                  , shouldInit = true
                  , transport  = rbus $ rs485 1 uart_0 out_pb_2
                  , features   = [ mix [ in_pa_4
                                       , in_pa_15
                                       , in_pb_3
                                       , in_pb_4
                                       , in_pb_5
                                       , in_pa_12
                                       , in_pa_9
                                       , in_pa_10
                                       , in_pa_8
                                       , in_pb_0
                                       , in_pb_1
                                       , in_pa_11
                                       ]
                                       [ out_pa_0
                                       , out_pa_1
                                       , out_pa_2
                                       , out_pa_3
                                       , out_pa_6
                                       , out_pa_7
                                       ]
                                       npx_pwm_0
                                       etc
                                 ]
                  }
