{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopG2 where

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
import Implementation.Smart.TopG2 (topG2)
import Implementation.Smart.TopG2v9 (topG2v9)
import Ivory.Language
import Transport.UART.RBUS

smartTopG2'v2 :: DFU GD32F3x0
smartTopG2'v2 =
    DFU
        { meta =
            Meta
                { name = "smart_top_g2"
                , model = deviceTypeSmartTopG2
                , board = 2
                , version = (4, 11)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbusTop uart_1
        , implementation =
            topG2
                ( dinputs $
                    in_pb_7
                        :> in_pb_3
                        :> Nil
                )
                (vibro out_pa_1)
                (powerTouch out_pa_8)
                (sht21 i2c_0)
                npx_pwm_1
                etc
        }

smartTopG2'v9 :: DFU GD32F3x0
smartTopG2'v9 =
    DFU
        { meta =
            Meta
                { name = "smart_top_g2"
                , model = deviceTypeSmartTopG2
                , board = 9
                , version = (4, 20)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbusTop uart_1
        , implementation =
            topG2v9
                ( touches glass $
                    touch_pa1
                        :> touch_pb0
                        :> Nil
                )
                (vibro out_pb_5)
                (sht21 i2c_0)
                npx_pwm_0
                etc
        }
