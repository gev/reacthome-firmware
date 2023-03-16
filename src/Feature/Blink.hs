{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}

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


blink :: Output o
      => Int
      -> (p -> o)
      -> WriterT Context (Reader (Domain p t)) Feature
blink n o = do
    mcu <- asks mcu
    let name  = "blink_" <> show n
    let out   = o $ peripherals mcu
    let state = value (name <> "_state") false
    let feature = Feature $ Blink { name, out, state }
    include state
    include out
    include $ delay 1_000 name $ do
        v <- deref $ addrOf state
        store (addrOf state) $ iNot v
        ifte_ v (set   out)
                (reset out)
    pure feature

instance Controller Blink
