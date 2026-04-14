{-# LANGUAGE NumericUnderscores #-}

module Formula.CardHolder where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Device.GD32F3x0.Touch (aluminum)
import Feature.DInputs
import Feature.Smart.Top.Vibro (vibro)
import Feature.Touches (touches)
import Implementation.CardHolder (cardHolder)
import Ivory.Language
import Transport.UART.RBUS

cardHolder'test :: DFU GD32F3x0
cardHolder'test =
    DFU
        { meta =
            Meta
                { name = "card_holder"
                , model = deviceTypeCardHolder
                , board = 1
                , version = (1, 0)
                , shouldInit = true
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                }
        , transport = rbusTop uart_1
        , implementation =
            cardHolder
                ( touches aluminum $
                    touch_pb1
                        :> touch_pb0
                        :> Nil
                )
                ( dinputs $
                    in_pb_6
                        :> Nil
                )
                (vibro out_pb_5)
                npx_pwm_0
                etc
        }

-- cardHolder'v1 :: DFU GD32F3x0
-- cardHolder'v1 =
--     DFU
--         { meta =
--             Meta
--                 { name = "card_holder"
--                 , model = deviceTypeCardHolder
--                 , board = 1
--                 , version = (1, 0)
--                 , shouldInit = true
--                 , mcu = gd32f330k8u6
--                 , quartzFrequency = 8_000_000
--                 , systemFrequency = 84_000_000
--                 }
--         , transport = rbusTop uart_1
--         , implementation =
--             cardHolder
--                 ( touches aluminum $
--                     touch_pa6
--                         :> touch_pa7
--                         :> Nil
--                 )
--                 ( dinputs $
--                     in_pb_6
--                         :> Nil
--                 )
--                 (vibro out_pb_5)
--                 npx_pwm_0
--                 etc
--         }
