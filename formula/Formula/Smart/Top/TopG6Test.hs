{-# LANGUAGE NumericUnderscores #-}

module Formula.Smart.Top.TopG6Test where

import Core.Formula
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.Touches (touches)
import Implementation.Smart.TopG6Test (topG6Test)
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS

smartTopG6Test :: Formula GD32F3x0
smartTopG6Test =
      Formula
            { name = "smart_top_g6test"
            , model = deviceTypeDoppler5Di4
            , version = (4, 8)
            , shouldInit = false
            , mcu = gd32f330k8u6
            , quartzFrequency = 8_000_000
            , systemFrequency = 84_000_000
            , implementation =
                  topG6Test
                        (rbus $ rs485 uart_1 out_pa_6)
                        ( touches 30 $
                              touch_pb8
                                    :> touch_pa15
                                    :> touch_pb7
                                    :> touch_pb3
                                    :> touch_pb5
                                    :> touch_pb4
                                    :> Nil
                        )
            }
