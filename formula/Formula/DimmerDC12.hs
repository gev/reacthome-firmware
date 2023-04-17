module Formula.DimmerDC12 where

import           Core.Formula
import           Device.GD32F3x0
import           Feature.DimmerDC
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS



dimmerDC12 :: Formula GD32F3x0
dimmerDC12 = Formula { name       = "dimmerDC12"
                     , model      = 0xff
                     , version    = (1, 0)
                     , shouldInit = false
                     , transport  = rbus $ rs485 1 uart_1 out_pa_4
                     , features   = [ dimmerDC [ out_pb_0
                                               , out_pa_5
                                               , out_pa_6
                                               , out_pa_0
                                               , out_pa_7
                                               , out_pa_1
                                               , out_pb_2
                                               , out_pa_8
                                               , out_pa_9
                                               , out_pa_10
                                               , out_pa_11
                                               , out_pa_12
                                               ]
                                    ]
                     }
