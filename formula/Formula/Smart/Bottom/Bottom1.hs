{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Bottom.Bottom1 where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F3x0
import           Feature.ALED                (ALED, aled)
import           Feature.DInputs
import           Feature.DS18B20
import           Feature.Sht21
import           Feature.Smart.Top
import           Implementation.Smart.Bottom (bottom1)
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS

smartBottom1 :: Formula GD32F3x0
smartBottom1 =  Formula { name            = "smart_bottom_1"
                        , model           = deviceTypeSmartBottom1
                        , version         = (4, 6)
                        , shouldInit      = true
                        , mcu             = gd32f330k8u6
                        , quartzFrequency =  8_000_000
                        , systemFrequency = 84_000_000
                        , implementation  = bottom1 (rbus $ rs485 uart_1 out_pa_4)
                                                    (top uart_0 in_pb_4)
                                                    (dinputs $  in_pa_5
                                                             :> in_pa_6
                                                             :> in_pb_0
                                                             :> in_pb_1
                                                             :> Nil
                                                    )
                                                    (ds18b20 ow_0 od_pa_15)
                                                    (aled npx_pwm_3 etc)

                        }
