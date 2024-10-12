module Formula.DimmerAC12 where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F3x0
import           Feature.Dimmers.AC
import           Feature.Indicator
import           Implementation.Dimmer (dimmer)
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS



dimmerAC12 :: Formula GD32F3x0
dimmerAC12 = Formula { name           = "dimmerAC12"
                     , model          = deviceTypeDim12AcRs
                     , version        = (2, 3)
                     , shouldInit     = true
                     , implementation = dimmer (rbus $ rs485 uart_0 out_pb_2)
                                               (dimmersAC (  pwm_11
                                                          :> pwm_7
                                                          :> pwm_6
                                                          :> pwm_8
                                                          :> pwm_10
                                                          :> pwm_9
                                                          :> pwm_0
                                                          :> pwm_1
                                                          :> pwm_2
                                                          :> pwm_3
                                                          :> pwm_4
                                                          :> pwm_5
                                                          :> Nil
                                                          )
                                                          exti_pa_5
                                                )
                                                (indicator npx_pwm_0 20)
                     }
