{-# LANGUAGE NumericUnderscores #-}

module Formula.Scd40 where

import           Core.Formula
import           Core.Models
import           Device.GD32F3x0
import           Feature.Scd40
import           Implementation.Dummy
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS

scd40_test :: Formula GD32F3x0
scd40_test = Formula { name            = "scd40"
                     , model           = deviceTypeCo2
                     , version         = (1, 0)
                     , shouldInit      = false
                     , quartzFrequency =  8_000_000
                     , systemFrequency = 84_000_000
                     , implementation  = dummy (rbus $ rs485 uart_1 out_pa_4)
                                               (scd40 i2c_0)
                     }
