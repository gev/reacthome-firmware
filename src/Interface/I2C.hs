{-# LANGUAGE GADTs #-}

module Interface.I2C where

import Core.Handler
import Data.Buffer
import Ivory.Language

type Address = Uint8

data HandleI2C i = HandleI2C
    { i2c :: i
    , handle :: forall n eff. Uint8 -> Uint16 -> Ivory eff ()
    }

class (Handler HandleI2C (i n)) => I2C i n where
    transmit :: i n -> Address -> Buffer n Uint8 -> Ivory eff ()
    receive :: i n -> Address -> Uint16 -> Ivory eff ()
