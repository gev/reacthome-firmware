module Formula.DI4RSM where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F3x0
import           Implementation.DiRsm    (diRsm)
import           Feature.DInputs
import           Feature.DS18B20
import           Feature.AOutputs       (aoutputs)
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS

di4rsm :: Formula GD32F3x0
di4rsm  = Formula { name           = "di4rsm"
                  , model          = deviceTypeDi4Rsm
                  , version        = (1, 0)
                  , shouldInit     = true
                  , implementation = diRsm (rbus $ rs485 uart_1 out_pa_5)
                                           (dinputs $  in_pa_12
                                                    :> in_pa_11
                                                    :> in_pa_10
                                                    :> in_pa_9
                                                    :> Nil
                                           )
                                           (aoutputs $  dac_pa_4 
                                                     :> Nil
                                           )
                                           (ds18b20 ow_0 od_pa_15)
                  }
