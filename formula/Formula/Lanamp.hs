{-# LANGUAGE NumericUnderscores #-}

module Formula.Lanamp where 

import           Core.Formula
import           Core.Models
import           Data.Fixed
import           Device.GD32F4xx
import qualified Implementation.Lanamp as I
import           Ivory.Language

lanamp :: Formula GD32F4xx
lanamp = Formula
    { name            = "lanamp"
    , model           = deviceTypeLanAmp
    , version         = (1, 0)
    , shouldInit      = false
    , mcu             = gd32f450vgt6
    , quartzFrequency =  24_000_000
    , systemFrequency = 192_000_000
    , implementation  = I.mkLanamp eth_0 i2s_tx_1
    }