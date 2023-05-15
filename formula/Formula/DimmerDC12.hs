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
                     , features   = [ dimmerDC [ pwm_0
                                               , pwm_1
                                               , pwm_2
                                               , pwm_3
                                               , pwm_4
                                               , pwm_5
                                               , pwm_6
                                               , pwm_7
                                               , pwm_8
                                               , pwm_9
                                               , pwm_10
                                               , pwm_11
                                               ]
                                    ]
                     }
