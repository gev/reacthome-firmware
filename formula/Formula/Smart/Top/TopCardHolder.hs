{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopCardHolder where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Device.GD32F3x0.Touch (aluminum)
import Feature.DInputs
import Feature.Smart.Top.Vibro (vibro)
import Feature.Touches (touches)
import Implementation.Smart.TopCardHolder (topCardHolder)
import Ivory.Language
import Transport.UART.RBUS


smartTopCardHolder'v1 :: DFU GD32F3x0
smartTopCardHolder'v1 =
    DFU
        { meta =
            Meta
                { name = "smart_top_card_holder"
                , model = deviceTypeSmartTopCardHolder
                , board = 1
                , version = (1, 0)
                , shouldInit = false
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbusTop uart_1
        , implementation =
            topCardHolder
                ( touches aluminum $
                    touch_pa6
                        :> touch_pa7
                        :> Nil
                )
                ( dinputsOffset $
                    in_pb_6
                        :> Nil
                )
                (vibro out_pb_5)
                npx_pwm_0
                etc
        }
