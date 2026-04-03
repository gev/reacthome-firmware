{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Bottom.Bottom where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.ALED (aled)
import Feature.DInputs
import Feature.DS18B20
import Feature.Smart.Top
import Implementation.Smart.Bottom (bottom)
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS

smartBottom'v13 :: DFU GD32F3x0
smartBottom'v13 =
    DFU
        { meta =
            Meta
                { name = "smart_bottom"
                , model = deviceTypeSmartBottom
                , board = 13
                , version = (4, 11)
                , shouldInit = true
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbus $ rs485 uart_1 out_pa_4
        , implementation =
            bottom
                (top uart_0 in_pb_4)
                ( dinputs $
                    in_pa_5
                        :> in_pa_6
                        :> in_pb_0
                        :> in_pb_1
                        :> Nil
                )
                (ds18b20 ow_0 od_pa_15)
                (aled npx_pwm_3 etc)
        }
