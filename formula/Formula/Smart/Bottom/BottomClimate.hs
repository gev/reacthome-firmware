{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Bottom.BottomClimate where

import Core.Formula.DFU
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.ALED (aled)
import Feature.DInputs
import Feature.DS18B20
import Feature.Sht21
import Implementation.Smart.BottomClimate (bottomClimate)
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS

smartBottomClimate :: DFU GD32F3x0
smartBottomClimate =
    DFU
        { name = "smart_bottom_climate"
        , model = deviceTypeSmartBottomClimate
        , version = (4, 12)
        , shouldInit = true
        , mcu = gd32f330k8u6
        , quartzFrequency = 8_000_000
        , systemFrequency = 84_000_000
        , transport = rbus $ rs485 uart_1 out_pa_4
        , implementation =
            bottomClimate
                ( dinputs $
                    in_pa_5
                        :> in_pa_6
                        :> in_pb_0
                        :> in_pb_1
                        :> Nil
                )
                (ds18b20 ow_0 od_pa_15)
                (sht21 i2c_0)
                (aled npx_pwm_3 etc)
        }
