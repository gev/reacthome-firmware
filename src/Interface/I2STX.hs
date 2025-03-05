{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interface.I2STX where 

import           Ivory.Language
import           Ivory.Language.Module
import           GHC.TypeNats
import           Core.Context
import           Core.Handler
import           Data.Buffer


newtype HandleI2STX i = HandleI2STX 
    { i2s    :: i
    
    }

class Handler HandleI2STX i => I2STX i  where
    
    transmit   :: i
               -> ((Uint32 -> forall eff. Ivory eff ()) -> Ivory (ProcEffects s t) ())
               -> Ivory (ProcEffects s t) ()

