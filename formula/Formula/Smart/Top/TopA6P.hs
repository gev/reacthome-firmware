module Formula.Smart.Top.TopA6P where

import           Core.Formula
import           Core.Models
import           Device.GD32F3x0
import           Feature.DInputs
import           Feature.Sht21 
import           Ivory.Language
import           Transport.UART.RBUS

smartTopA6P :: Formula GD32F3x0
smartTopA6P =  Formula { name       = "smart-top-a6p"
                       , model      = deviceTypeSmartTopA6P
                       , version    = (1, 0)
                       , shouldInit = false
                       , transport  = rbus uart_0
                       , features   = [ sht21 i2c_0 0x80
                                      , dinputs [ in_pa_4
                                                , in_pb_2
                                                , in_pa_5
                                                , in_pb_1
                                                , in_pa_6
                                                , in_pb_0
                                                ]
                                      ]
                       }
