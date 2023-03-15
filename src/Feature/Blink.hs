{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}

module Feature.Blink where

import           Control.Monad.Reader
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
      => Int -> (p -> o) -> Reader (Domain p t) Feature
blink n out = do
    mcu <- asks mcu
    pure $ Feature $ Blink { name  = name
                           , out   = out $ peripherals mcu
                           , state = value (name <> "_state") false
                           }
    where name = "blink_" <> show n



instance Include Blink where
    include (Blink {..}) = do
        include state
        include out
        include $ delay 1_000 name $ do
            v <- deref $ addrOf state
            store (addrOf state) $ iNot v
            ifte_ v (set   out)
                    (reset out)



instance Controller Blink
