{-# LANGUAGE NumericUnderscores #-}

module Formula.Blink330 where

import           Core.Formula
import           Device.GD32F3x0
import           Implementation.Blink    (blink)
import           Interface.RS485
import           Ivory.Language
import           Support.Device.GD32F3x0
import           Transport.RS485.RBUS


blink330 :: Formula GD32F3x0
blink330 = Formula { name           = "blink330"
                   , model          = 0xff
                   , version        = (1, 0)
                   , shouldInit     = false
                   , implementation = blink out_pa_15 timer_15
                   }
