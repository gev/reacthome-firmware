{-# LANGUAGE NumericUnderscores #-}

module Formula.MixW where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Data.Fixed
import Device.GD32F3x0
import Feature.DInputs (dinputs)
import Feature.IndicatorFlush
import Feature.Relays (relays)
import Implementation.MixW (mix)
import Interface.Etc (Etc (..))
import Interface.RS485
import Ivory.Language
import Transport.RS485.RBUS

mixW'v1 :: DFU GD32F3x0
mixW'v1 =
    DFU
        { meta =
            Meta
                { name = "mixW"
                , model = deviceTypeMixW
                , board = 1
                , version = (3, 0)
                , shouldInit = true
                , mcu = gd32f330k8u6
                , quartzFrequency = 8_000_000
                , systemFrequency = 84_000_000
                , etc = Just Etc{version = 1, etc = 0x0800_bc00}
                }
        , transport = rbus $ rs485 uart_0 out_pb_2
        , implementation =
            mix
                ( dinputs $
                    in_pb_5
                        :> in_pb_4
                        :> in_pb_3
                        :> in_pa_15
                        :> in_pa_8
                        :> in_pa_9
                        :> in_pa_10
                        :> in_pa_11
                        :> Nil
                )
                ( relays $
                    out_pa_0
                        :> out_pa_1
                        :> out_pa_6
                        :> out_pa_7
                        :> out_pb_0
                        :> out_pb_1
                        :> Nil
                )
            (indicator npx_pwm_0 140)
        }

-- inputs:
-- in_pb_5
-- in_pb_4
-- in_pb_3
-- in_pa_15
-- in_pa_8
-- in_pa_9
-- in_pa_10
-- in_pa_11

-- relay:
-- out_pa_0
-- out_pa_1
-- out_pa_6
-- out_pa_7
-- out_pb_0
-- out_pb_1

-- rs485:
-- rede: pb_2
-- rx: pb_7
-- tx: pb_6
