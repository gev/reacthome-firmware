module Formula.Doppler5 where

import           Core.Formula
import           Core.Models
import           Device.GD32F3x0
import           Feature.DInputs     
import           Feature.Dopplers         
import           Implementation.Doppler hiding (dinputs, dopplers)
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS

doppler5 :: Formula GD32F3x0
doppler5 = Formula { name           = "doppler5"
                    , model          = 0xff
                    , version        = (1, 0)
                    , shouldInit     = false
                    , transport      = rbus $ rs485 uart_1 out_pa_4
                    , implementation = doppler (dopplers [adc_pa_0])
                                               (dinputs  [ in_pa_12
                                                         , in_pa_11
                                                         , in_pa_10
                                                         , in_pa_9
                                                         ]
                                               )
                    }
