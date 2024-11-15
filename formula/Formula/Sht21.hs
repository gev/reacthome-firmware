{-# LANGUAGE NumericUnderscores #-}

module Formula.Sht21 where

import           Core.Formula
import           Core.Models
import           Device.GD32F3x0
import           Feature.Sht21
import           Implementation.Dummy
import           Ivory.Language
import           Transport.UART.RBUS

sht21_test :: Formula GD32F3x0
sht21_test = Formula { name            = "sht21"
                     , model           = 0xFF
                     , version         = (1, 0)
                     , shouldInit      = false
                     , quartzFrequency =  8_000_000
                     , systemFrequency = 84_000_000
                     , implementation  = dummy (rbus uart_0 1_000_000)
                                               (sht21 i2c_0)
                     }
