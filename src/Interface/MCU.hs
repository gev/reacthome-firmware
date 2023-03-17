{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

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
    { family       :: String
    , model        :: String
    , modification :: String
    , hasFPU       :: Bool
    , systemClock  :: SystemClock
    , peripherals  :: p
    , mac          :: Mac
    }


mcu :: Monad m
    => String
    -> Bool
    -> WriterT Context m SystemClock
    -> (Buffer 6 Uint8 -> forall eff. Ivory eff ())
    -> ModuleM ()
    -> p
    -> String
    -> String
    -> WriterT Context m (MCU p)
mcu family hasFPU systemClock' initializeMac mcuModule peripherals model modification = do
    include mcuModule
    systemClock <- systemClock'
    mac         <- makeMac initializeMac "mac"
    pure MCU { family, model, modification, hasFPU, systemClock, peripherals, mac }
