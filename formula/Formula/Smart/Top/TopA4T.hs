{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopA4T where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Device.GD32F3x0.Touch (aluminum)
import Feature.DInputs (dinputs)
import Feature.Sht21 (sht21)
import Feature.Smart.Top.PowerTouch (powerTouch)
import Feature.Smart.Top.Vibro (vibro)
import Feature.Touches (touches)
import Implementation.Smart.TopA4T (topA4T)
import Implementation.Smart.TopA4Tv5 (topA4Tv5)
import Ivory.Language
import Transport.UART.RBUS

smartTopA4T :: DFU GD32F3x0
smartTopA4T =
    DFU
        { meta =
            Meta
                { name = "smart_top_a4t"
                , model = deviceTypeSmartTopA4T
                , board = 0
                , version = (4, 10)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbusTop uart_1
        , implementation =
            topA4T
                ( dinputs $
                    in_pb_4
                        :> in_pb_8
                        :> in_pb_5
                        :> in_pb_7
                        :> Nil
                )
                (vibro out_pa_1)
                (powerTouch out_pa_8)
                (sht21 i2c_0)
                npx_pwm_1
                etc
        }

smartTopA4Tv5 :: DFU GD32F3x0
smartTopA4Tv5 =
    DFU
        { meta =
            Meta
                { name = "smart_top_a4t"
                , model = deviceTypeSmartTopA4T
                , board = 5
                , version = (4, 19)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbusTop uart_1
        , implementation =
            topA4Tv5
                ( touches aluminum $
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
