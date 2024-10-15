{-# LANGUAGE NumericUnderscores #-}
module Formula.Smart.Top.TopA4P where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F3x0
import           Feature.DInputs             (dinputs)
import           Feature.Sht21               (sht21)
import           Implementation.Smart.TopA4P (topA4P)
import           Ivory.Language
import           Transport.UART.RBUS         (rbus)

smartTopA4P :: Formula GD32F3x0
smartTopA4P =  Formula { name           = "smart_top_a4p"
                       , model          = deviceTypeSmartTopA4P
                       , version        = (4, 5)
                       , shouldInit     = false
                       , implementation = topA4P (rbus uart_0 115_200)
                                                 (dinputs $  in_pa_4
                                                         :> in_pb_2
                                                         :> in_pa_5
                                                         :> in_pb_1
                                                         :> Nil
                                                 )
                                                 (sht21 i2c_0)
                                                 npx_pwm_0
                                                 etc
                       }
