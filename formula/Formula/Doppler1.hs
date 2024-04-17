module Formula.Doppler1 where

import           Core.Formula
import           Core.Models
import           Device.GD32F3x0
import           Feature.DInputs
import           Feature.Dopplers
import           Implementation.Doppler hiding (dinputs, dopplers)
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS

doppler1 :: Formula GD32F3x0
doppler1 = Formula { name           = "doppler1"
                   , model          = deviceTypeDoppler1Di4
                   , version        = (1, 0)
                   , shouldInit     = false
                   , implementation = doppler (rbus $ rs485 uart_1 out_pa_4)
                                              (dopplers [ adc_pa_7
                                                        ]
                                              )
                                              (dinputs  [ in_pa_12
                                                        , in_pa_11
                                                        , in_pa_10
                                                        , in_pa_9
                                                        ]
                                              )
                    }
