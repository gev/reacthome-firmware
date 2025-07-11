{-# LANGUAGE NumericUnderscores #-}

module Formula.DimmerDC12 where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F3x0
import           Feature.Dimmers.DC
import           Feature.Indicator
import           Implementation.DimmerDIN (dimmer)
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS



dimmerDC12 :: Formula GD32F3x0
dimmerDC12 = Formula { name            = "dimmerDC12"
                     , model           = deviceTypeDim12DcRs
                     , version         = (2, 9)
                     , shouldInit      = true
                     , mcu             = gd32f330k8u6
                     , quartzFrequency =  8_000_000
                     , systemFrequency = 84_000_000
                     , implementation  = dimmer (rbus $ rs485 uart_0 out_pb_2)
                                                (dimmersDC $  pwm_9
                                                           :> pwm_10
                                                           :> pwm_8
                                                           :> pwm_6
                                                           :> pwm_7
                                                           :> pwm_11
                                                           :> pwm_5
                                                           :> pwm_4
                                                           :> pwm_3
                                                           :> pwm_2
                                                           :> pwm_1
                                                           :> pwm_0
                                                           :> Nil
                                                 )
                                                 (indicator npx_pwm_0 240)
                     }
