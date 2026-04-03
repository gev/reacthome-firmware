{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Formula.DI4LA where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.ALED (aled)
import Feature.DInputs
import Feature.DS18B20
import Implementation.DILA (dila)
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS

di4la'v3 :: DFU GD32F3x0
di4la'v3 =
    DFU
        { meta =
            Meta
                { name = "di4la"
                , model = deviceTypeDi4La
                , board = 3
                , version = (1, 10)
                , shouldInit = true
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbus $ rs485 uart_1 out_pa_4
        , implementation =
            dila
                ( dinputs $
                    in_pa_12
                        :> in_pa_11
                        :> in_pa_10
                        :> in_pa_9
                        :> Nil
                )
                (ds18b20 ow_1 od_pa_8)
                (aled npx_pwm_0 etc)
        }
