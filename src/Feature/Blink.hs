{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}

module Feature.Blink where

import           Control.Monad.Reader
import           Control.Monad.Writer
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

data Blink where
    Blink :: Output o
          => { name  :: String
             , out   :: o
             , state :: Value IBool
             } -> Blink


blink :: (MonadWriter Context m, MonadReader (Domain p t) m, Output o)
      => Int -> (p -> m o) -> m Feature
blink n o = do
    let name  = "blink_" <> show n

    mcu      <- asks mcu
    out      <- o $ peripherals mcu
    state    <- value (name <> "_state") false

    let feature = Feature $ Blink { name, out, state }

    addTask $ delay 1_000 name $ do
        v <- deref state
        store state $ iNot v
        ifte_ v (set   out)
                (reset out)

    pure feature

instance Controller Blink
