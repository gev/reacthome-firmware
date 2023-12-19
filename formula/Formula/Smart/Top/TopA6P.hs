module Formula.Smart.Top.TopA6P where

import           Core.Formula
import           Device.GD32F3x0
import           Feature.DInputs
import           Ivory.Language
import           Transport.UART.RBUS

smartTopA6P ::  Formula GD32F3x0
smartTopA6P =   Formula { name       = "smart-top-a6p"
                        , model      = 0x20
                        , version    = (1, 0)
                        , shouldInit = false
                        , transport  = rbus uart_1
                        , features   = [ dinputs [  in_pa_12
                                                 ,  in_pa_11
                                                 ,  in_pa_10
                                                 ,  in_pa_9
                                                 ]
                                       ]
                        }
