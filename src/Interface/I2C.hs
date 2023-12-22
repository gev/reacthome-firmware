{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Interface.I2C where
    
import Ivory.Language
import Core.Handler
import Data.Buffer


type Address = Uint8

data HandleI2C i = HandleI2C
    { i2c  :: i
    , handle :: forall eff. Ivory eff ()
    }

class Handler HandleI2C i => I2C i where
    transmit :: i -> Address -> Buffer n Uint8 -> Ivory eff ()
    receive  :: i -> Address -> Uint32 -> Ivory eff ()