{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Interface.I2SRX where 

import           Ivory.Language
import           Ivory.Language.Module
import           GHC.TypeNats
import           Core.Context
import           Core.Handler
import           Data.Buffer
import           Interface.I2S


data HandleI2SRX i = HandleI2SRX 
    { i2s    :: i 
    , handle  :: forall eff. (Uint32, Uint32) -> Ivory eff ()
    }


