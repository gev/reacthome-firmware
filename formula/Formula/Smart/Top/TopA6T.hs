{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopA6T where

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
import Implementation.Smart.TopA6T (topA6T)
import Implementation.Smart.TopA6Tv5 (topA6Tv5)
import Ivory.Language
import Transport.UART.RBUS

smartTopA6T'3 :: DFU GD32F3x0
smartTopA6T'3 =
    DFU
        { meta =
            Meta
                { name = "smart_top_a6t"
                , model = deviceTypeSmartTopA6T
                , board = 3
                , version = (4, 11)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbusTop uart_1
        , implementation =
            topA6T
                ( dinputs $
                    in_pa_15
                        :> in_pb_7
                        :> in_pb_3
                        :> in_pb_6
                        :> in_pb_4
                        :> in_pb_5
                        :> Nil
                )
                (vibro out_pa_4)
                (powerTouch out_pa_8)
                (sht21 i2c_0)
                npx_pwm_1
                etc
        }

smartTopA6T'v5 :: DFU GD32F3x0
smartTopA6T'v5 =
    DFU
        { meta =
            Meta
                { name = "smart_top_a6t"
                , model = deviceTypeSmartTopA6T
                , board = 5
                , version = (4, 20)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbusTop uart_1
        , implementation =
            topA6Tv5
                ( touches aluminum $
                    touch_pa0
                        :> touch_pb1
                        :> touch_pa1
                        :> touch_pb0
                        :> touch_pa6
                        :> touch_pa7
                        :> Nil
                )
                (vibro out_pb_5)
                (sht21 i2c_0)
                npx_pwm_0
                etc
        }
