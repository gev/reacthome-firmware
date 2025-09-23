{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interface.I2SRX where

import Core.Context
import Core.Handler
import Data.Buffer
import GHC.TypeNats
import Interface.I2S
import Ivory.Language
import Ivory.Language.Module

data HandleI2SRX i = HandleI2SRX
    { i2s :: i
    , handle :: forall eff. Sample -> Ivory eff ()
    }
