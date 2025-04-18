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


data HandleI2SRX i = HandleI2SRX 
    { i2s    :: i 
    , handle  :: forall n eff. Uint32 -> forall eff. Ivory eff ()
    }


