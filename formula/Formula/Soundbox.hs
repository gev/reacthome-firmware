{-# LANGUAGE NumericUnderscores #-}

module Formula.Soundbox where


import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F4xx
import qualified Feature.Lanamp          as S
import qualified Implementation.Soundbox as I
import           Ivory.Language




soundbox :: Formula GD32F4xx
soundbox = Formula
    { name            = "soundbox"
    , model           = deviceTypeLanAmp
    , version         = (1, 0)
    , shouldInit      = false
    , mcu             = gd32f450vgt6
    , quartzFrequency =  24_000_000
    , systemFrequency = 192_000_000
    , implementation  = I.soundbox i2s_trx_1 i2c_2 out_pd_12
    }
