{-# LANGUAGE NumericUnderscores #-}

module Formula.Soundbox where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F4xx
import  Implementation.Soundbox
import           Ivory.Language



soundbox :: Formula GD32F4xx
soundbox = Formula
    { name            = "soundbox"
    , model           = deviceTypeLanAmp
    , version         = (1, 0)
    , shouldInit      = false
    , mcu             = gd32f450vit6
    , quartzFrequency =  24_000_000
    , systemFrequency = 192_000_000
    , implementation  = mkSoundbox eth_0 i2s_trx_1 out_pd_12 i2s_tx_2 out_pb_7 i2c_2 out_pc_2
    }
