{-# LANGUAGE NumericUnderscores #-}

module Formula.SoundboxLS where

import Core.Formula.DFU
import Core.Meta
import Core.Models
import Device.GD32F4xx
import Implementation.SoundboxLS (mkSoundboxLS)
import Ivory.Language
import Transport.UDP.RBUS qualified as U

soundboxLS'v7 :: DFU GD32F4xx
soundboxLS'v7 =
    DFU
        { meta =
            Meta
                { name = "soundbox_ls"
                , board = 7
                , model = deviceTypeSoundboxLS
                , version = (1, 0)
                , shouldInit = true
                , mcu = gd32f450vit6
                , quartzFrequency = 24_000_000
                , systemFrequency = 192_000_000
                , etc = Nothing
                }
        , transport = U.rbus eth_0
        , implementation =
            mkSoundboxLS
                eth_0
                i2s_trx_1
                out_pd_12
                i2s_trx_2
                out_pb_7
                i2c_2
                out_pc_2
        }
