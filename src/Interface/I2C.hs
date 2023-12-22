{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.I2C where

import           Core.Handler
import           Data.Buffer
import           Ivory.Language


type Address = Uint8

data HandleI2C i = HandleI2C
    { i2c    :: i
    , handle :: forall eff. Uint8 -> Uint16 -> Ivory eff ()
    }

class Handler HandleI2C i => I2C i where
    transmit :: i -> Address -> Buffer n Uint8 -> Ivory eff ()
    receive  :: i -> Address -> Uint16 -> Ivory eff ()
