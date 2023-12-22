{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

module Feature.Blink where

import           Control.Monad.Reader
import           Control.Monad.State   (MonadState)
import           Core.Context
import           Core.Controller
import           Core.Domain
import           Core.Feature
import           Core.Task
import           Data.Value
import           Interface.GPIO.Output
import           Interface.MCU         (MCU (peripherals))
import           Interface.Timer
import           Ivory.Language
import Interface.GPIO.Port

data Blink where
    Blink :: Output o
          => { name  :: String
             , out   :: o
             , state :: Value IBool
             } -> Blink


blink :: (MonadState Context m, MonadReader (Domain p t) m, Output o, Pull p u)
      => Int -> (p -> u -> m o) -> m Feature
blink n o = do
    let name  = "blink_" <> show n

    mcu      <- asks mcu
    let peripherals' = peripherals mcu
    out      <- o peripherals' $ pullNone peripherals'
    state    <- value (name <> "_state") false

    let feature = Feature $ Blink { name, out, state }

    addTask $ delay 1_000 name $ do
        v <- deref state
        store state $ iNot v
        ifte_ v (set   out)
                (reset out)

    pure feature

instance Controller Blink
