{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}

module Interface.MCU where

import           Control.Monad.Writer
import           Core.Context
import           Data.Buffer
import           Data.Value
import           Interface.Mac
import           Interface.SystemClock (SystemClock)
import           Ivory.Language
import           Ivory.Language.Module


data MCU p = MCU
    { model        :: String
    , modification :: String
    , systemClock  :: SystemClock
    , peripherals  :: p
    , mac          :: Mac
    }


mcu :: MonadWriter Context m
    => m SystemClock
    -> (Buffer 6 Uint8 -> forall eff. Ivory eff ())
    -> ModuleDef
    -> p
    -> String
    -> String
    -> m (MCU p)
mcu systemClock' initializeMac mcuModule peripherals model modification = do
    addModule mcuModule
    systemClock <- systemClock'
    mac         <- makeMac initializeMac "mac"
    pure MCU { model, modification, systemClock, peripherals, mac }
