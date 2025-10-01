module Interface.I2SRX where

import Interface.I2S
import Ivory.Language

data HandleI2SRX i = HandleI2SRX
    { i2s :: i
    , handle :: forall eff. Sample -> Ivory eff ()
    }
