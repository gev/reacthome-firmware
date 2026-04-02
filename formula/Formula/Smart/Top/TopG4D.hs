{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopG4D where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Device.GD32F3x0.Touch (glass)
import Feature.DInputs (dinputs)
import Feature.Sht21 (sht21)
import Feature.Smart.Top.PowerTouch (powerTouch)
import Feature.Smart.Top.Vibro (vibro)
import Feature.Touches (touches)
import Implementation.Smart.TopG4Dv15 (topG4Dv15)
import Implementation.Smart.TopGD (topGD)
import Ivory.Language
import Transport.UART.RBUS

smartTopG4D :: DFU GD32F3x0
smartTopG4D =
    DFU
        { meta =
            Meta
                { name = "smart_top_g4d"
                , model = deviceTypeSmartTopG4D
                , board = 0
                , version = (4, 10)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbusTop uart_1
        , implementation =
            topGD
                ( dinputs $
                    in_pb_7
                        :> in_pb_5
                        :> in_pb_8
                        :> in_pb_4
                        :> Nil
                )
                (vibro out_pa_1)
                (powerTouch out_pa_8)
                (sht21 i2c_0)
                npx_pwm_1
                etc
        }

smartTopG4Dv15 :: DFU GD32F3x0
smartTopG4Dv15 =
    DFU
        { meta =
            Meta
                { name = "smart_top_g4d"
                , model = deviceTypeSmartTopG4D
                , board = 15
                , version = (4, 19)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbusTop uart_1
        , implementation =
            topG4Dv15
                ( touches glass $
                    touch_pa6
                        :> touch_pb1
                        :> touch_pa7
                        :> touch_pb0
                        :> Nil
                )
                (vibro out_pb_5)
                (sht21 i2c_0)
                npx_pwm_0
                etc
        }
