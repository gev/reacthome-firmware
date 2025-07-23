{-# LANGUAGE NumericUnderscores #-}

module Formula.MixH where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F3x0
import           Feature.Dimmers.DC    (dimmersDC)
import           Feature.DInputs       (dinputs)
import           Feature.DS18B20       (ds18b20)
import           Feature.Relays        (relays)
import           Implementation.MixH   (mix)
import           Interface.RS485
import           Ivory.Language
import           Transport.RS485.RBUS


mixH :: Formula GD32F3x0
mixH = Formula  { name            = "mixH"
                , model           = deviceTypeMixH
                , version         = (3, 0)
                , shouldInit      = true
                , mcu             = gd32f330k8u6
                , quartzFrequency =  8_000_000
                , systemFrequency = 84_000_000
                , implementation  = mix (rbus $ rs485 uart_0 out_pb_2)
                                        (dinputs $ in_pa_3
                                                :> in_pa_2
                                                :> in_pa_1
                                                :> in_pa_0
                                                :> in_pb_5
                                                :> in_pb_4
                                                :> in_pb_3
                                                :> in_pa_15
                                                :> Nil
                                        )
                                        (relays $ out_pa_11
                                               :> out_pa_10
                                               :> Nil
                                        )
                                        (dimmersDC $ pwm_9
                                                  :> pwm_8
                                                  :> pwm_7
                                                  :> pwm_6
                                                  :> pwm_5
                                                  :> pwm_4
                                                  :> Nil
                                        )
                                        (ds18b20 ow_0 od_pa_5)
                                        etc
                }

{--

transports: [rbus $ rs485 uart_0 out_pb_2]

di's: [pa3, pa2, pa1, pa0, pb5, pb4, pb3, pa15]

relays: [pa11, pa10]

dimmers: [pwm_9, pwm_8, pwm_7, pwm_6, pwm_5, pwm_4]

1w: pa5

--}


