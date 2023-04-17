module Formula.DimmerAC12 where

import           Core.Formula
import           Device.GD32F3x0
import           Feature.DimmerAC
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS


dimmerAC12 :: Formula GD32F3x0
dimmerAC12 = Formula { name       = "dimmerAC12"
                     , model      = 0xff
                     , version    = (1, 0)
                     , shouldInit = false
                     , transport  = rbus $ rs485 1 uart_1 out_pa_4
                     , features   = [ dimmerAC [ out_pb_0
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
                                               ] in_pa_13
                                    ]
                     }
