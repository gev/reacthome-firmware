{-# LANGUAGE NumericUnderscores #-}

module Formula.DimmerAC1 where

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



dimmerAC1 :: Formula GD32F3x0
dimmerAC1 =  Formula { name            = "dimmerAC1"
                     , model           = deviceTypeDim1AcRs
                     , version         = (2, 2)
                     , shouldInit      = true
                     , mcu             = gd32f330k8u6
                     , quartzFrequency =  8_000_000
                     , systemFrequency = 84_000_000
                     , implementation  = dimmer (rbus $ rs485 uart_1 out_pa_4)
                                                (dimmersAC (  pwm_11
                                                           :> Nil 
                                                           )
                                                           exti_pb_7
                                                 )
                     }
