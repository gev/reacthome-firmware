{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopA2T where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Device.GD32F3x0.Touch (aluminumRoomNumber)
import Feature.Sht21 (sht21)
import Feature.Smart.Top.Vibro (vibro)
import Feature.Touches (touches)
import Implementation.Smart.TopA2Tv5 (topA2Tv5)
import Interface.Etc (Etc (..))
import Ivory.Language
import Transport.UART.RBUS

smartTopA2T'v5 :: DFU GD32F3x0
smartTopA2T'v5 =
    DFU
        { meta =
            Meta
                { name = "smart_top_a2t"
                , model = deviceTypeSmartTopA2T
                , board = 5
                , version = (4, 22)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                , etc = Just Etc{version = 1, etc = 0x0800_bc00}
                }
        , transport = rbusTop uart_1
        , implementation =
            topA2Tv5
                ( touches aluminumRoomNumber $
                    touch_pa6
                        :> touch_pb1
                        :> Nil
                )
                (vibro out_pb_5)
                (sht21 i2c_0)
                npx_pwm_0
        }
