module Formula.DimmerDC12 where

import           Core.Formula
import           Core.Models
import           Device.GD32F3x0
import           Feature.Dimmer.DC    (dimmerDC)
import           Feature.Indicator
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS



dimmerDC12 :: Formula GD32F3x0
dimmerDC12 = Formula { name       = "dimmerDC12"
                     , model      = deviceTypeDim12DcRs
                     , version    = (2, 1)
                     , shouldInit = true
                     , transport  = rbus $ rs485 1 uart_0 out_pb_2
                     , features   = [ indicator npx_pwm_0 240
                                    , dimmerDC [ pwm_9
                                               , pwm_10
                                               , pwm_8
                                               , pwm_6
                                               , pwm_7
                                               , pwm_11
                                               , pwm_5
                                               , pwm_4
                                               , pwm_3
                                               , pwm_2
                                               , pwm_1
                                               , pwm_0
                                               ]
                                    ]
                     }
