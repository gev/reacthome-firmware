{-# LANGUAGE NumericUnderscores #-}

module Formula.Blink450 where

import           Core.Formula
import           Device.GD32F4xx
import           Implementation.Blink (blink)
import           Ivory.Language
import           Transport.UART.RBUS


blink450 :: Formula GD32F4xx
blink450 = Formula { name            = "blink450"
                   , model           = 0xff
                   , version         = (1, 0)
                   , shouldInit      = false
                   , mcu             = gd32f450vgt6
                   , quartzFrequency =  25_000_000
                   , systemFrequency = 200_000_000
                   , implementation  = blink out_pa_8
                   }
