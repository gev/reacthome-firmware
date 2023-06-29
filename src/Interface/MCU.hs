{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.MCU where

import           Control.Monad.Writer
import           Core.Context
import           Data.Buffer
import           Data.Value
import           Interface.Flash
import           Interface.Mac
import           Interface.SystemClock (SystemClock)
import           Ivory.Language
import           Ivory.Language.Module


data MCU p = MCU
    { systemClock :: SystemClock
    , peripherals :: p
    , mac         :: Mac
    }


data MCUmod p = MCUmod
    { mcu          :: forall m. MonadWriter Context m => m (MCU p)
    , model        :: String
    , modification :: String
    }


mkMCU :: (MonadWriter Context m)
      => m SystemClock
      -> (Buffer 6 Uint8 -> forall s. Ivory (ProcEffects s ()) ())
      -> ModuleDef
      -> p
      -> m (MCU p)
mkMCU systemClock' initializeMac mcuModule peripherals = do
    addModule mcuModule
    systemClock <- systemClock'
    mac         <- makeMac initializeMac "mac"
    pure MCU { systemClock, peripherals, mac }
