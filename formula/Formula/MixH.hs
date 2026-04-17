{-# LANGUAGE NumericUnderscores #-}

module Formula.MixH where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.DInputs (dinputs)
import Feature.DS18B20 (ds18b20)
import Feature.Dimmers.DC (dimmersDC)
import Feature.Relays (relays)
import Implementation.MixH (mix)
import Implementation.MixHv6 (mix'v6)
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS
import Feature.IndicatorFlush

mixH'v3 :: DFU GD32F3x0
mixH'v3 =
    DFU
        { meta =
            Meta
                { name = "mixH"
                , model = deviceTypeMixH
                , board = 3
                , version = (3, 3)
                , shouldInit = true
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbus $ rs485 uart_0 out_pb_2
        , implementation =
            mix
                ( dinputs $
                    in_pa_3
                        :> in_pa_2
                        :> in_pa_1
                        :> in_pa_0
                        :> in_pb_5
                        :> in_pb_4
                        :> in_pb_3
                        :> in_pa_15
                        :> Nil
                )
                ( relays $
                    out_pa_11
                        :> out_pa_10
                        :> Nil
                )
                ( dimmersDC $
                    pwm_9
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

mixH'v6 :: DFU GD32F3x0
mixH'v6 =
    DFU
        { meta =
            Meta
                { name = "mixH"
                , model = deviceTypeMixH
                , board = 6
                , version = (4, 0)
                , shouldInit = true
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbus $ rs485 uart_0 out_pb_2
        , implementation =
            mix'v6
                ( dinputs $
                    in_pa_3
                        :> in_pa_2
                        :> in_pa_1
                        :> in_pa_0
                        :> in_pb_5
                        :> in_pb_4
                        :> in_pb_3
                        :> in_pa_15
                        :> Nil
                )
                ( relays $
                    out_pa_11
                        :> out_pa_10
                        :> Nil
                )
                ( dimmersDC $
                    pwm_9
                        :> pwm_8
                        :> pwm_7
                        :> pwm_6
                        :> pwm_5
                        :> pwm_4
                        :> Nil
                )
                (ds18b20 ow_2 od_pa_5)
                (indicator npx_pwm_0 140)
                etc
        }

{--

transports: [rbus $ rs485 uart_0 out_pb_2]

di's: [pa3, pa2, pa1, pa0, pb5, pb4, pb3, pa15]

relays: [pa11, pa10]

dimmers: [pwm_9, pwm_8, pwm_7, pwm_6, pwm_5, pwm_4]

1w: pa5

--}
