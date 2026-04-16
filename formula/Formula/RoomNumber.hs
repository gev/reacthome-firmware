module Formula.RoomNumber where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Device.GD32F3x0.Touch (aluminum)
import Feature.Smart.Top.Vibro (vibro)
import Feature.Touches
import Implementation.RoomNumber (roomNumber)
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS

roomNumber'test :: DFU GD32F3x0
roomNumber'test =
    DFU
        { meta =
            Meta
                { name = "room_number_test"
                , model = deviceTypeRoomNumber
                , board = 1
                , version = (1, 0)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbus $ rs485 uart_1 out_pa_6
        , implementation =
            roomNumber
                ( touches aluminum $
                    touch_pb1
                        :> Nil
                )
                (vibro out_pb_5)
                npx_pwm_0
                etc
        }

roomNumber'v1 :: DFU GD32F3x0
roomNumber'v1 =
    DFU
        { meta =
            Meta
                { name = "room_number"
                , model = deviceTypeRoomNumber
                , board = 1
                , version = (1, 0)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbus $ rs485 uart_1 out_pa_4
        , implementation =
            roomNumber
                ( touches aluminum $
                    touch_pa6
                        :> Nil
                )
                (vibro out_pb_5)
                npx_pwm_0
                etc
        }