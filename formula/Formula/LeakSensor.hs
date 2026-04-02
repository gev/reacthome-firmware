module Formula.LeakSensor where

import Core.Formula
import Core.Meta
import Device.GD32F3x0
import Implementation.LeakSensor as L
import Ivory.Language

leakSensor :: Formula GD32F3x0
leakSensor =
    Formula
        { meta =
            Meta
                { name = "leak_sensor"
                , model = 0
                , board = 0
                , version = (1, 0)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , implementation =
            L.leakSensor
                adc_pa_1
                out_pa_2
                out_pa_3
                npx_pwm_0
        }
