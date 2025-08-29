{-# LANGUAGE NumericUnderscores #-}

module Formula.Soundbox where

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F4xx
import           Implementation.Soundbox (mkSoundbox)
import           Ivory.Language
import qualified Transport.UDP.RBUS      as U



soundbox :: Formula GD32F4xx
soundbox = Formula
    { name            = "soundbox"
    , model           = deviceTypeSoundbox
    , version         = (1, 0)
    , shouldInit      = true
    , mcu             = gd32f450vit6
    , quartzFrequency =  24_000_000
    , systemFrequency = 192_000_000
    , implementation  = mkSoundbox (U.rbus eth_0) eth_0 i2s_trx_1 out_pd_12 i2s_tx_2 out_pb_7 i2c_2 out_pc_2
    }
